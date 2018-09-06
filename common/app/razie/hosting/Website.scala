package razie.hosting

import play.api.mvc.{Request, RequestHeader}
import razie.OR._
import razie.wiki.Services
import razie.wiki.model._
import razie.wiki.util.{DslProps, PlayTools}

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

  def homePage:Option[WID] = this wprop "home"
  def userHomePage:Option[WID] = this wprop "userHome"

  def notifyList:Option[WID] = this wprop "notifyList"
  def footer:Option[WID] = this wprop "footer"

  /** blog URL - can be http://xxx or CAT:NAME */
  def blog:String = {
    this prop "blog" flatMap {b=>
      if(b startsWith "http") Some(b)
      else this wprop "blog" map (_.url)
    } OR ""
  }

  //WID("Blog", "RacerKidz_Site_News").url

  def twitter:String = this prop "twitter" OR "racerkid"
  def gplus:Option[String] = this prop "gplus"
  def tos:String = this prop "tos" OR "/wiki/Terms_of_Service"
  def privacy:String = this prop "privacy" OR "/wiki/Privacy_Policy"

  def dieselReactor:String = this prop "dieselReactor" OR reactor
  def dieselVisiblity:String = this prop "diesel.visibility" OR "public"
  def dieselTrust:String = this prop "diesel.trust" OR ""

  def stylesheet:Option[WID] = this wprop "stylesheet"

  def join:String = this prop "join" OR "/doe/join"
  def parent:Option[WID] = this wprop "parent"
  def skipParent:Option[WID] = this wprop "skipParent"

  def divMain:String = this prop "divMain" OR "9"
  def copyright:Option[String] = this prop "copyright"

  def logo:Option[String] = this prop "logo"

  def adsOnList = this bprop "adsOnList" OR true
  def adsAtBottom = this bprop "adsAtBottom" OR true
  def adsOnSide = this bprop "adsOnSide" OR true
  def adsForUsers = this bprop "adsForUsers" OR true
  def noadsForPerms = (this prop "noAdsForPerms").map(_.split(",")) OR Array.empty[String]

  def openMembership         = this bprop "users.openMembership" OR true
  def membersCanCreateTopics = this bprop "users.membersCanCreateTopics" OR true

  def rightTop:Option[WID] = this wprop "rightTop"
  def rightBottom:Option[WID] = this wprop "rightBottom"
  def about:Option[String] = this prop "about" flatMap {s=>
    if (s.startsWith("http") || (s startsWith "/")) Some(s)
    else WID.fromPath(s).map(_.url)
  }

  def userTypes:List[String] = (this prop "userTypes").map(_.split(",").toList).getOrElse(Services.config.userTypes)
  def userTypeDesc(ut:String):Option[String] = this prop ("userType."+ut)

  def layout:String = this prop "layout" OR "Play:classicLayout"


  def useWikiPrefix:Boolean = this bprop "useWikiPrefix" OR true

  //todo optimize - don't parse every time
  def propFilter (prefix:String) = {
    propSeq.filter(_._1 startsWith (prefix)).map(t=>(t._1.replaceFirst(prefix, ""), t._2))
  }

  //sections should be "More" "Support" "Social"
  def bottomMenu (section:String) = propFilter(s"bottom.$section.")

  def navrMenu () = propFilter(s"navr.")
  def navrMenuRemove () = propFilter(s"navr.remove.").map(_._1)

  def navMenu () =
    propFilter(s"nav.") ++
      Seq("admin.badgeRefreshAllTests" -> "/diesel/statusAll")

  def metas () = propFilter(s"meta.")

  def navTheme:String = this prop "nav.Theme" OR "/doe/selecttheme"
  def navBrand = this prop "navBrand"

  def supportUrl:String = this prop "support.url" OR "/doe/support"
  def supportEmail = this prop "support.email" OR "support@racerkidz.com"
  def SUPPORT2 = this prop "support.email" OR "support@effectiveskiing.com"
}

/** multihosting utilities */
object Website {
  private case class CacheEntry (w:Website, millis:Long)
  private val cache = new collection.mutable.HashMap[String,CacheEntry]()
  val EXP = 100000

  /** cache existing sites - I get stupid lookups for sites that don't exist... and result in many lookups */
  private val sites = new collection.mutable.HashMap[String,String]()

  def lookupSite (s:String) : Option[WikiEntry] = {
    synchronized {
      if(sites.isEmpty) {
//         init
        Wikis.rk.pageNames("Site").toList map (s=>sites.put(s,s))
      }
    }

    if(sites.contains(s)) Wikis.rk.find("Site", s)
    else None
  }

  // s is the host
  def forHost (s:String):Option[Website] = {
    val ce = cache.get(s)
    if (ce.isEmpty) {// || System.currentTimeMillis > ce.get.millis) {
      var w = lookupSite(s) map (new Website(_))
      if (w.isDefined)
        cache.put(s, CacheEntry(w.get, System.currentTimeMillis()+EXP))
      else RkReactors.forHost(s).map { r=>
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
      val web = WikiReactors(r).we.map(we=> new Website(we))
      web.foreach {w=>
        cache.put(w.domain, CacheEntry(w, System.currentTimeMillis()+EXP))
      }
      web
    }
  }

  def clean (host:String):Unit = {
    cache.remove(host)
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
