/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.model

import com.mongodb.DBObject
import com.mongodb.casbah.Imports._
import com.novus.salat._
import razie.audit.Audit
import razie.{Logging, clog}
import razie.db.RazMongo
import razie.db.RazSalatContext._
import razie.diesel.engine.DieselAppContext
import razie.hosting.Website
import razie.wiki.util.DslProps
import razie.wiki.{Services, WikiConfig}

import scala.collection.mutable.ListBuffer

/**
  * reactor management (multi-tenant) - we can host multiple wikis/websites, each is a "reactor"
  *
  * this holds all the reactors hosted in this process
  */
object WikiReactors extends Logging {
  // reserved reactors
  final val RK = WikiConfig.RK       // original, still decoupling code
  final val WIKI = "wiki"              // main reactor
  final val NOTES = WikiConfig.NOTES

  final val ALIASES = Map ("www" -> "wiki")

  /** lower case index - reactor names are insensitive */
  val lowerCase = new collection.mutable.HashMap[String,String]()

  // loaded in Global
  val reactors = new collection.mutable.HashMap[String,Reactor]()

  // all possible reactors, some loaded some not
  val allReactors = new collection.mutable.HashMap[String,WikiEntry]()

  @volatile var loading = false

  private def loadReactors(): Unit = synchronized {
    if(loading) {
      Audit.logdb("DEBUG-WARNING", "Already Loading reactors " + Thread.currentThread().getName)
      return
    }

    loading = true

    //todo - sharding... now all realms currently loaded in this node
    val res = reactors

    // load reserved reactors: rk and wiki first

    var toload = Services.config.preload.split(",")

    // list all reactors to be loaded and pre-fetch wikis
    //todo with large numbers this will leak
    RazMongo(Wikis.TABLE_NAME)
      .find(Map("category" -> "Reactor"))
      .map(grater[WikiEntry].asObject(_))
      .toList
      .foreach(we=>allReactors.put(we.name, we))

    // load the basic reactors ahead of everyone that might depend on them
    if(toload contains RK) loadReactor(RK)
    if(toload contains NOTES) { // todo does not have a wiki - damn, can't use loadReactor
        res.put (NOTES, Services.mkReactor(NOTES, Nil, None))
        lowerCase.put(NOTES, NOTES)
    }
    if(toload contains WIKI) loadReactor(WIKI)

    // todo this will create issues such that for a while after startup things are weird
    razie.Threads.fork {
      synchronized {
        try {
          // the basic were already loaded, will be ignored
          Audit.logdb("DEBUG", "Loading reactors " + Thread.currentThread().getName)

          Services.config.preload
            .split(",")
            .foreach(loadReactor(_, None))

          // now load the rest
          val rest = allReactors.filter(x => !reactors.contains(x._1)).map(_._1)
          rest.foreach(loadReactor(_, None))

        } catch {
          case t: Throwable =>
            error("while loading reactors", t)
            Audit.logdb("DEBUG-ERR", "EXCEPTION loading reactors " + t)
        }

        Audit.logdb("DEBUG", "DONE Loaded reactors " + Thread.currentThread().getName)

      }
      // all loaded - start other problematic services
      DieselAppContext.start
    }
  }

  /** lazy load a reactor
    *
    * @param r
    * @param useThis when reloading a new version
    */
  private def loadReactor(r:String, useThis:Option[WikiEntry] = None, reload:Boolean=false) : Unit = synchronized {
      if (!reload && lowerCase.contains(r.toLowerCase) && useThis.isEmpty) return;

    try {
      var toLoad = new ListBuffer[WikiEntry]()
      toLoad append useThis.getOrElse(allReactors(r))

      val max = 20 // linearized mixins max
      var curr = 0

      // lazy depys
      while (curr < max && !toLoad.isEmpty) {
        curr += 1
        val copy = toLoad.toList
        toLoad.clear()

        // todo smarter linearization of mixins
        copy.foreach { we =>
          val mixins = getMixins(Some(we))

          if (mixins.foldLeft(true) { (a, b) => a && lowerCase.contains(b.toLowerCase) }) {
            // all mixins are loaded, go ahead
            clog << "LOADING REACTOR " + we.wid.name
            reactors.put(we.name, Services.mkReactor(we.name, mixins.toList.map(x => reactors(x)), Some(we)))
            lowerCase.put(we.name.toLowerCase, we.name)
          } else {
            clog << s"NEED TO LOAD LATER REACTOR ${we.wid.name} depends on ${mixins.mkString(",")}"
            toLoad appendAll mixins.filterNot(x => lowerCase.contains(x.toLowerCase)).map(allReactors.apply)
            toLoad += we
          }
        }
      }
    } catch {
      case t: Throwable =>
        error("while loading reactor "+r, t)
        Audit.logdb("DEBUG-ERR", "EXCEPTION loading reactor " + r + " - " + t)
    }
  }

  /** find the mixins from realm properties */
  private def getMixins (we:Option[WikiEntry]) =
    new DslProps(we, "website,properties")
      .prop("mixins")
      .map(_.split(","))
      .getOrElse{
        // the basic cannot depend on anyone other than what they want
        if(we.exists(we=> Array(RK,NOTES,WIKI) contains we.name)) Array.empty[String] else Array(WIKI)
      }

  def findWikiEntry(r:String) =
    rk.wiki.weTable("WikiEntry")
      .findOne(Map("category" -> "Reactor", "name" -> r))
      .map(grater[WikiEntry].asObject(_))

  def reload(r:String): Unit = synchronized  {
    // can't remove them first, so we can reload RK reactor
    findWikiEntry(r).foreach{we=>
      // first, refresh the loaded copy
      allReactors.put(r, we)

      // todo no mixins? just wiki ?
      //      reactors.put (we.name, Services.mkReactor(we.name, List(wiki), Some(we)))

      // then reload
      loadReactor(r, Some(we), true)
    }
  }

  def rk = reactors(RK)
  def wiki = reactors(WIKI)

  def add (realm:String, we:WikiEntry): Reactor = synchronized  {
    assert(! reactors.contains(realm), "Sorry, SITE_ERR: Reactor already active ???")
    val r = Services.mkReactor(realm, getMixins(Some(we)).flatMap(reactors.get).toList, Some(we))
    reactors.put(realm, r)
    lowerCase.put(realm.toLowerCase, realm)
    r
  }

  def contains (realm:String) : Boolean = reactors.contains(realm)

  def apply (realm:String = Wikis.RK) : Reactor = {
    // preload
    if(reactors.isEmpty) loadReactors()
    // anything not preloaded, load now
    if(!reactors.contains(realm) && allReactors.contains(realm)) loadReactor(realm)
    reactors.getOrElse(realm, rk)
  } // using RK as a fallback

  lazy val fallbackProps = new DslProps(WID("Reactor", "wiki").r("wiki").page, "properties,properties")

  WikiObservers mini {
    case WikiEvent(_, "WikiEntry", _, Some(x), _, _, _) if fallbackProps.we.exists(_.uwid == x.asInstanceOf[WikiEntry].uwid) => {
      fallbackProps.reload(x.asInstanceOf[WikiEntry])
    }

    case WikiEvent(_, "WikiEntry", _, Some(x), _, _, _)
      if x.isInstanceOf[WikiEntry] &&
        x.asInstanceOf[WikiEntry].category == "Reactor" => {
      val we = x.asInstanceOf[WikiEntry]
      razie.audit.Audit.logdb("DEBUG", "event.reloadreactor", we.wid.wpath)
      loadReactor(we.name, Some(we))
//        WikiReactors.reload(we.name);
        Website.clean (we.name+".dieselapps.com")
        new Website(we).prop("domain").map (Website.clean)
      }
  }

  WikiIndex.init()
}


