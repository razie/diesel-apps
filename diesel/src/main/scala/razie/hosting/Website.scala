/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.hosting

import play.api.mvc.{Request, RequestHeader}
import razie.OR._
import razie.diesel.dom.RDOM.P
import razie.wiki.Services
import razie.wiki.model._
import razie.wiki.util.{DslProps, PlayTools}
import scala.collection.concurrent.TrieMap
import scala.collection.mutable.HashMap
import scala.collection.parallel.mutable

/**
 * Multihosting - website settings - will collect website properties from the topic if it has a 'website' section
 */
class Website (we:WikiPage, extra:Seq[(String,String)] = Seq()) extends DslProps(Some(we), "website,properties", extra) {
  def label:String = this prop "label" OR name
  def name:String = this prop "name" OR we.name //"-"
  def reactor:String = this prop "reactor" OR (this prop "realm" OR "rk")

  def title = this prop "title"
  def css:Option[String] = this prop "css" // dark vs light

  /** make a url for this realm - either configured */
  def url:String = this prop "url" OR ("http://" + domain)

  def domain:String = this prop "domain" OR (s"$name.dieselapps.com")

  def homePage:Seq[(String, String)] = (this prop "home").map("home" -> _).toSeq ++  propFilter(s"home.")
  def userHomePage:Option[WID] = this wprop "userHome"

  lazy val trustedSites:Array[String] = (this prop "trustedSites" OR "").split(",")

  def notifyList:Option[WID] = this wprop "notifyList"
  def footer:Option[WID] = this wprop "footer"

  def twitter:String = this prop "bottom.Connect.Twitter" OR "coolscala"
  def tos:String = this prop "tos" OR "/wiki/Terms_of_Service"
  def privacy:String = this prop "privacy" OR "/wiki/Privacy_Policy"

  def dieselReactor:String = this prop "dieselReactor" OR reactor
  def dieselVisiblity:String = this prop "diesel.visibility" OR "public"
  def dieselTrust:String = this prop "diesel.trust" OR ""
  def dieselEnvList:String = this prop "diesel.envList" OR ""

  def stylesheet:Option[WID] = this wprop "stylesheet"

  def join:String = this prop "join" OR "/doe/join"

  def divMain:String = this prop "divMain" OR "9"
  def copyright:Option[String] = this prop "copyright"

  def logo:Option[String] = this prop "logo"

  def adsOnList = this bprop "adsOnList" OR true
  def adsAtBottom = this bprop "adsAtBottom" OR true
  def adsOnSide = this bprop "adsOnSide" OR true
  def adsForUsers = this bprop "adsForUsers" OR true
  def noadsForPerms = (this prop "noAdsForPerms").map(_.split(",")) OR Array.empty[String]
  def needsConsent = this bprop "needsConsent" OR true

  def openMembership         = this bprop "users.openMembership" OR true
  def membersCanCreateTopics = this bprop "users.membersCanCreateTopics" OR true

  def rightTop:Option[WID] = this wprop "rightTop"
  def rightBottom:Option[WID] = this wprop "rightBottom"
  def about:Option[String] = this prop "bottom.More.About" flatMap {s=>
    if (s.startsWith("http") || (s startsWith "/")) Some(s)
    else WID.fromPath(s).map(_.url)
  }

  def userTypes:List[String] = (this prop "userTypes").map(_.split(",").toList).getOrElse(Services.config.userTypes)
  def userTypeDesc(ut:String):Option[String] = this prop ("userType."+ut)

  def layout:String = this prop "layout" OR "Play:classicLayout"
  def kind = this prop "kind"


  def useWikiPrefix:Boolean = this bprop "useWikiPrefix" OR true

  //todo optimize - don't parse every time
  // find properties with prefix and list them
  def propFilter (prefix:String) = {
    propSeq.filter(_._1 startsWith (prefix)).map(t=>(t._1.replaceFirst(prefix, ""), t._2))
  }

  //sections should be "More" "Support" "Social"
  def bottomMenu (section:String) = propFilter(s"bottom.$section.")

  def navrMenu () = propFilter(s"navr.")
  def navrMenuRemove () = propFilter(s"navr.remove.").map(_._1)

  def navMenu () =
    propFilter(s"nav.")

  def metas () = propFilter(s"meta.")

  def navBrand = this prop "navBrand"

  def supportUrl:String = this prop "bottom.Support.Support" OR "/doe/support"
  def adminEmail = this prop "admin.email" OR "razie@razie.com"
  def supportEmail = this prop "support.email" OR "support@racerkidz.com"
  def SUPPORT2 = this prop "support.email" OR "support@effectiveskiing.com"
}

/** multihosting utilities */
object Website {
  private case class CacheEntry (w:Website, millis:Long)
  private case class CacheEntry2 (m:collection.mutable.HashMap[String,P])

  // cleaned on reload
  private val cache = new TrieMap[String, CacheEntry]()

  // static
  private val realmProps = new TrieMap[String, CacheEntry2]()

  val EXP = 100000

  // s is the host
  def forHost (s:String):Option[Website] = {
    val ce = cache.get(s)

    if (ce.isEmpty) {// || System.currentTimeMillis > ce.get.millis) {
      var w : Option[Website] = None

      RkReactors.forHost(s).map { r=>

        // auto-websites of type REACTOR.coolscala.com
        WikiReactors.findWikiEntry(r).map { rpage=> // todo no need to reload, the reactor now has the page
          // create an entry even if no website section present
          w = Some(new Website(rpage, Seq("reactor" -> r)))
          cache.put(s, CacheEntry(w.get, System.currentTimeMillis()+EXP))
        }
      }
      w
    } else
      ce.map(_.w)
  }

  def forRealm (r:String):Option[Website] = {
    cache.values.find(_.w.reactor == r).map(_.w).orElse {
      val web = WikiReactors.maybeFind(r).flatMap(_.we).map(we=> new Website(we))
      web.foreach {w=>
        cache.put(w.domain, CacheEntry(w, System.currentTimeMillis()+EXP))
      }
      web
    }
  }

  def clean (host:String):Unit = {
    cache.remove(host)
  }

  /** static properties per realm, as set during say diesel.realm.started
    * make sure P is calculated to a value - no context from here on
    */
  def putRealmProps (realm:String, prop:String, value:P):Unit = {
    if(!value.hasCurrentValue) {
      throw new IllegalArgumentException("P needs a calculatedValue")
    }
    // important to sync here, conflicts are not harmless
    realmProps.synchronized {
      val p = realmProps.getOrElseUpdate(realm, new CacheEntry2(new HashMap[String, P]()))
      p.m.put(prop, value)
    }
  }

  /** static properties per realm, as set during say diesel.realm.started */
  def getRealmProp (realm:String, prop:String, dfltValue:Option[String] = None) = {
    realmProps.get(realm).flatMap(_.m.get(prop).map(_.currentStringValue)).orElse(dfltValue)
  }

  /** static properties per realm, as set during say diesel.realm.started */
  def getRealmProps (realm:String):Map[String,P] = {
   realmProps.get(realm).map(_.m.toMap).getOrElse(Map.empty)
  }

  def all = cache.values.map(_.w).toList

  def xrealm   (implicit request:RequestHeader) = (getHost flatMap Website.forHost).map(_.reactor).getOrElse(dflt.reactor)
  def realm    (implicit request:Request[_]) = apply(request).map(_.reactor).getOrElse(dflt.reactor)
  def getRealm (implicit request:Request[_]) = realm(request)

  def apply    (implicit request: Request[_]):Option[Website] = getHost flatMap Website.forHost

  def userTypes (implicit request: Request[_]) = apply(request).map(_.userTypes).getOrElse(Services.config.userTypes)
  def userTypeDesc(ut:String)(implicit request: Request[_]) : String = apply(request).flatMap(_.userTypeDesc(ut)).getOrElse(ut)

  /** find or default */
  def get  (implicit request: Request[_]) : Website = apply getOrElse dflt

  def dflt = new Website(Wikis.rk.categories.head) //todo this is stupid - find better default

  /** @deprecated use PlayTools.getHost */
  def getHost (implicit request: RequestHeader) = PlayTools.getHost
}

