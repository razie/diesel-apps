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
import razie.clog
import razie.db.RazSalatContext._
import razie.db.RazMongo
import razie.wiki.util.DslProps
import razie.wiki.{Services, WikiConfig}
import razie.diesel.dom.WikiDomain

import scala.collection.mutable.ListBuffer

/**
  * reactor management (multi-tenant) - we can host multiple wikis/websites, each is a "reactor"
  *
  * this holds all the reactors hosted in this process
  */
object WikiReactors {
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

  private def loadReactors() = {
    //todo - sharding... now all realms currently loaded in this node
    val res = reactors

    // load reserved reactors: rk and wiki first

    var toload = Services.config.preload.split(",")

    // list all reactors to be loaded and pre-fetch wikis
    //todo with large numbers this will leak
    RazMongo(Wikis.TABLE_NAME).find(Map("category" -> "Reactor")).map(
      grater[WikiEntry].asObject(_)).toList.foreach(
      we=>allReactors.put(we.name, we))

    // load the basic reactors ahead of everyone that might depend on them
    if(toload contains RK) loadReactor(RK)
    if(toload contains NOTES) { // todo does not have a wiki - damn, can't use loadReactor
        res.put (NOTES, Services.mkReactor(NOTES, Nil, None))
        lowerCase.put(NOTES, NOTES)
    }
    if(toload contains WIKI) loadReactor(WIKI)

    // todo this will create issues such that for a while after startup things are weird
    razie.Threads.fork {
      // the basic were already loaded, will be ignored
      Services.config.preload.split(",").foreach(loadReactor)
    }
  }

  /** lazy load a reactor */
  private def loadReactor(r:String) : Unit = {

    if(lowerCase.contains(r.toLowerCase)) return;

    val res = reactors
    var toLoad = new ListBuffer[WikiEntry]()
    toLoad append allReactors(r)

    val max = 20 // linearized mixins max
    var curr = 0

      // lazy depys
    while (curr < max && !toLoad.isEmpty) {
      curr += 1
      val copy = toLoad.toList
      toLoad.clear()

      // todo smarter linearization of mixins
      copy.foreach {we=>
        val mixins = new DslProps(Some(we), "website").prop("mixins").map(_.split(",")).getOrElse{
          // the basic cannot depend on anyone other than what they want
          if(Array(RK,NOTES,WIKI) contains we.name) Array.empty[String] else Array("wiki")
        }

        if(mixins.foldLeft(true){(a,b)=> a && lowerCase.contains(b.toLowerCase)}) {
          clog << "LOADING REACTOR " + we.wid.name
          res.put (we.name, Services.mkReactor(we.name, mixins.toList.map(x=> res.get(x).get), Some(we)))
          lowerCase.put(we.name.toLowerCase, we.name)
        } else {
          clog << s"NEED TO LOAD LATER REACTOR ${we.wid.name} depends on ${mixins.mkString(",")}"
          toLoad appendAll mixins.filterNot(x=>lowerCase.contains(x.toLowerCase)).map(allReactors.apply)
          toLoad += we
        }
      }
    }
  }

  def findWikiEntry(r:String) =
    rk.wiki.weTable("WikiEntry").findOne(Map("category" -> "Reactor", "name" -> r)).map(grater[WikiEntry].asObject(_))

  def reload(r:String): Unit = {
    // can't remove them first, so we can reload RK reactor
    findWikiEntry(r).foreach{we=>
      reactors.put (we.name, Services.mkReactor(we.name, List(wiki), Some(we)))
      lowerCase.put(we.name.toLowerCase, we.name)
    }
  }

  def rk = reactors(RK)
  def wiki = reactors(WIKI)

  def add (realm:String, we:WikiEntry): Reactor = {
    assert(! reactors.contains(realm), "Sorry, SITE_ERR: Reactor already active ???")
    val r = Services.mkReactor(realm, reactors.get(WIKI).toList, Some(we))
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

  // todo listen to updates and reload
  lazy val fallbackProps = new DslProps(WID("Reactor", "wiki").r("wiki").page, "properties")

  WikiObservers mini {
    case WikiEvent(_, "WikiEntry", _, Some(x), _, _, _) if fallbackProps.we.exists(_.uwid == x.asInstanceOf[WikiEntry].uwid) => {
      fallbackProps.reload(x.asInstanceOf[WikiEntry])
    }
  }

  WikiIndex.init()
}

/** a hosted wiki instance, i.e. independent hosted website.
  *
  * It has its own index, domain and is independent of other wikis
  *
  * It has its own users and admins/mods etc
  *
  * Wikis can mixin other wikis - linearized multiple inheritance.
  */
class Reactor (val realm:String, val fallBacks:List[Reactor] = Nil, val we:Option[WikiEntry]) {
  val wiki   : WikiInst   = new WikiInst(realm, fallBacks.map(_.wiki))
  val domain : WikiDomain = new WikiDomain (realm, wiki)

  val mixins = new Mixins[Reactor](fallBacks)
  lazy val club = props.wprop("club").flatMap(Wikis.find)

  lazy val userRoles = websiteProps.prop("userRoles").toList.flatMap(_.split(','))
  lazy val adminEmails = websiteProps.prop("adminEmails").toList.flatMap(_.split(','))

  // list of super reactors linearized
  val supers : Array[String] = {
    if(realm == WikiReactors.RK) Array(WikiReactors.RK)
    else mixins.flatten.map(_.realm).toArray
  }

  /** Admin:UserHome if user or Admin:Home or Reactor:realm if nothing else is defined */
  def mainPage(au:Option[WikiUser]) = {
    def dflt = WID("Admin", "UserHome").r(realm).page.map(_.wid)
//    val p = au.flatMap {user=>
//      if(adminEmails.contains(Dec(user.email)))
//        WID("Admin", "AdminHome").r(realm).page.map(_.wid) orElse dflt
//      else
//        club.flatMap(c=> user.myPages(c.realm, "Club").find(_.uwid.id == c._id)).flatMap {uw=>
//          WID("Admin", uw.role+"Home").r(realm).page.map(_.wid) orElse dflt
//        } orElse
//          dflt
      val p = au.flatMap {user=>
        dflt
    } orElse WID("Admin", "Home").r(realm).page.map(_.wid) getOrElse WID("Reactor", realm).r(realm)
    p
  }

  def sectionProps(section:String) = {
    we.orElse(WID("Reactor", realm).r(realm).page).map{p=>
      new DslProps(Some(p), "website")
    } getOrElse
      WikiReactors.fallbackProps
  }

  lazy val websiteProps = sectionProps("website")

  //todo fallback also in real time to rk, per prop
  // todo listen to updates and reload
  lazy val props = {
    we.orElse(WID("Reactor", realm).r(realm).page).map{p=>
      new DslProps(Some(p), "properties")
    } getOrElse
      WikiReactors.fallbackProps
  }

  WikiObservers mini {
    case WikiEvent(_, "WikiEntry", _, Some(x), _, _, _) if props.we.exists(_.uwid == x.asInstanceOf[WikiEntry].uwid) => {
      props.reload(x.asInstanceOf[WikiEntry])
    }
  }
}


