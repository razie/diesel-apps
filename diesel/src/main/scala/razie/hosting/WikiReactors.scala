/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.hosting

import com.mongodb.casbah.Imports._
import com.novus.salat._
import razie.Logging
import razie.audit.Audit
import razie.db.RazMongo
import razie.db.RazSalatContext._
import razie.diesel.engine.DieselAppContext
import razie.diesel.model.{DieselMsg, DieselTarget, ScheduledDieselMsg}
import razie.tconf.hosting.Reactors
import razie.wiki.model._
import razie.wiki.util.DslProps
import razie.wiki.{Services, WikiConfig}
import scala.collection.mutable.ListBuffer
import scala.concurrent.{Future, Promise}
import scala.util.Try

/**
  * reactor management (multi-tenant) - we can host multiple wikis/websites, each is a "reactor"
  *
  * this holds all the reactors hosted in this process
  */
object WikiReactors extends Logging with Reactors {
  // reserved reactors
  final val RK = WikiConfig.RK       // original, still decoupling code
  final val WIKI = "wiki"              // main reactor
  final val SPECS = "specs"              // main reactor
  final val NOTES = WikiConfig.NOTES

  final val ALIASES = Map ("www" -> "specs")

  /** lower case index of loaded reactors - reactor names are insensitive */
  val lowerCase = new collection.mutable.HashMap[String,String]()

  // loaded in Global
  val reactors = new collection.mutable.HashMap[String,Reactor]()

  // todo mt-safe use collection.concurrent.TrieMap ?

  // all possible reactors, some loaded some not
  val allReactors = new collection.mutable.HashMap[String,WikiEntry]()
  var allLowercase = List[String]()

  /** find the mixins from realm properties */
  private def getMixins (we:Option[WikiEntry]) =
    new DslProps(we, "website,properties")
        .prop("mixins")
        .map(_
            .split(",")
            .filter(_.trim.length > 0))
        .filter(_.size > 0) // not mixins = NOTHING
        .getOrElse{
          // the basic cannot depend on anyone other than what they want
          if(we.exists(we=> Array(RK,NOTES,WIKI) contains we.name)) Array.empty[String]
          else Array(WIKI)
        }

  def findWikiEntry(name:String, cat:String = "Reactor") =
    rk.wiki.weTable("WikiEntry")
        .findOne(Map("category" -> cat, "name" -> name))
        .map(grater[WikiEntry].asObject(_))

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

  def maybeFind (realm:String = Wikis.RK) : Option[Reactor] = {
    reactors.get(realm)
  }

  override def getProperties (realm:String) : Map[String,String] = {
    apply(realm).websiteProps.props
  }


  // ====================================== loading

  // stays on even after they're loaded
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
    //todo with large numbers of reactors, this will leak
    RazMongo(Wikis.TABLE_NAME)
      .find(Map("category" -> "Reactor"))
      .map(grater[WikiEntry].asObject(_))
      .toList
      .foreach(we=>allReactors.put(we.name, we))

    allLowercase = allReactors.keySet.map(_.toLowerCase).toList

    // filter toload and keep only what's actually there - on localhost, not all are there
    toload = toload.filter(allLowercase.contains)

    clog <<
        s"""
          |=======================================================
          |          Preloading reactors: ${toload.mkString}
          |=======================================================
          |""".stripMargin

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

          toload.foreach(loadReactor(_, None))

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
      clog <<
          """
            |=======================================================
            |
            |                Reactors loaded
            |
            |=======================================================
            |""".stripMargin
      DieselAppContext.start
      reactorsLoadedP.success(true)
    }
  }

  private val reactorsLoadedP : Promise[Boolean] = Promise[Boolean]()

  /** wait here if you need reactors */
  val reactorsLoadedF : Future[Boolean] = reactorsLoadedP.future

  /** lazy load a reactor
    *
    * @param r
    * @param useThis when reloading a new version
    */
  private def loadReactor(r:String, useThis:Option[WikiEntry] = None, reload:Boolean=false) : Unit = synchronized {
    clog <<
        s"""
           |===========  loadReactor: $r
           |""".stripMargin

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
          val realm = we.wid.name

          if (mixins.foldLeft(true) { (a, b) => a && lowerCase.contains(b.toLowerCase) }) {
            // all mixins are loaded, go ahead
            clog << "LOADING REACTOR " + realm
            val re = Services.mkReactor(we.name, mixins.toList.map(x => reactors(x)), Some(we))
            reactors.put(we.name, re)
            lowerCase.put(we.name.toLowerCase, we.name)

            // send realm.loaded message if anyone used it
            if(re.wiki.find("Spec", "EnvironmentSettings").exists(_.content.contains(DieselMsg.REALM.REALM_LOADED))) {
            Services ! ScheduledDieselMsg("10 seconds", DieselMsg(
              DieselMsg.REALM.ENTITY,
              DieselMsg.REALM.LOADED,
              Map("realm" -> realm),
              DieselTarget.ENV(realm)
            ))
            }
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

  lazy val fallbackProps = new DslProps(WID("Reactor", "wiki").r("wiki").page, "properties,properties")

  WikiObservers mini {
    // todo on remote nodes, load the Some(x) from id
    case WikiEvent(_, "WikiEntry", _, Some(x), _, _, _) => {

      // reload fallbacks when mixins change
      if (fallbackProps.we.exists(_.uwid == x.asInstanceOf[WikiEntry].uwid)) {
        fallbackProps.reload(x.asInstanceOf[WikiEntry])
      }

      // reload reactor when changes
      if (x.isInstanceOf[WikiEntry] && x.asInstanceOf[WikiEntry].category == "Reactor") {
        val we = x.asInstanceOf[WikiEntry]
        razie.audit.Audit.logdb("DEBUG", "event.reloadreactor", we.wid.wpath)
        loadReactor(we.name, Some(we))
        //        WikiReactors.reload(we.name);
        Website.clean(we.name + ".dieselapps.com")
        new Website(we).prop("domain").map(Website.clean)
      }
    }
  }

  WikiIndex.init()
}


