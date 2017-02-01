package model

import controllers.StateOk
import play.api.mvc.{Request, RequestHeader}
import razie.OR._
import razie.wiki.Services
import razie.wiki.model._
import razie.wiki.util.{DslProps, PlayTools}

/**
 * Multihosting - website settings - will collect website properties from the topic if it has a 'website' section
 */
class Website (we:WikiEntry, extra:Seq[(String,String)] = Seq()) extends DslProps(Some(we), "website", extra) {
  def label:String = this prop "label" OR name
  def name:String = this prop "name" OR "-"
  def title = this prop "title"
  def url:String = this prop "url" OR "-"
  def css:Option[String] = this prop "css" // dark vs light

  def homePage:Option[WID] = this wprop "home"
  def userHomePage:Option[WID] = this wprop "userHome"

  def notifyList:Option[WID] = this wprop "notifyList"
  def footer:Option[WID] = this wprop "footer"

  /** blog URL - can be http://xxx or CAT:NAME */
  def blog:String = {
    this prop "blog" flatMap {b=>
      if(b startsWith "http") Some(b)
      else this wprop "blog" map (_.url)
    } getOrElse WID("Blog", "RacerKidz_Site_News").url
  }

  def twitter:String = this prop "twitter" OR "racerkid"
  def gplus:Option[String] = this prop "gplus"
  def tos:String = this prop "tos" OR "/wiki/Terms_of_Service"
  def privacy:String = this prop "privacy" OR "/wiki/Privacy_Policy"
  def support:String = this prop "support" OR "/doe/support"

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

  def rightTop:Option[WID] = this wprop "rightTop"
  def rightBottom:Option[WID] = this wprop "rightBottom"
  def about:Option[String] = this prop "about" flatMap {s=>
    if (s.startsWith("http") || (s startsWith "/")) Some(s)
    else WID.fromPath(s).map(_.url)
  }

  def userTypes:List[String] = (this prop "userTypes").map(_.split(",").toList).getOrElse(Services.config.userTypes)
  def userTypeDesc(ut:String):Option[String] = this prop ("userType."+ut)

  def layout:String = this prop "layout" OR "Play:classicLayout"

  def reactor:String = this prop "reactor" OR (this prop "realm" OR "rk")

  def useWikiPrefix:Boolean = this bprop "useWikiPrefix" OR true

  //sections should be "More" "Support" "Social"
  def bottomMenu (section:String) = {
    propSeq.filter(_._1 startsWith (s"bottom.$section")).map(t=>(t._1.replaceFirst(s"bottom.$section.", ""), t._2))
  }

  //nav.TopLevel
  def navrMenu () = {
    propSeq.filter(_._1 startsWith (s"navr.")).map(t=>(t._1.replaceFirst(s"navr.", ""), t._2))
  }

  //nav.TopLevel
  def navMenu () = {
    propSeq.filter(_._1 startsWith (s"nav.")).map(t=>(t._1.replaceFirst(s"nav.", ""), t._2))
  }

  def metas () = {
    propSeq.filter(_._1 startsWith (s"meta.")).map(t=>(t._1.replaceFirst(s"meta.", ""), t._2))
  }

    def navTheme:String = this prop "nav.Theme" OR "/doe/selecttheme"
    def navBrand = this prop "navBrand"
}

/** multihosting utilities */
object Website {
  private case class CacheEntry (w:Website, millis:Long)
  private val cache = new collection.mutable.HashMap[String,CacheEntry]()
  val EXP = 100000

  // s is the host
  def apply (s:String):Option[Website] = {
    val ce = cache.get(s)
    if (ce.isEmpty) {// || System.currentTimeMillis > ce.get.millis) {
      var w = Wikis.rk.find("Site", s) map (new Website(_))
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

  def forRealm (r:String) = WikiReactors(r).we.map(we=> new Website(we))
  def forReactor (we:WikiEntry) = new Website(we) // todo optimize and not create every time

  def all = cache.values.map(_.w).toList

  def xrealm (implicit request:RequestHeader) = (getHost flatMap Website.apply).map(_.reactor).getOrElse(dflt.reactor)
  def realm (implicit request:Request[_]) = apply(request).map(_.reactor).getOrElse(dflt.reactor)
  def getRealm (implicit request:Request[_]) = realm(request)

  def apply (implicit request: Request[_]):Option[Website] = getHost flatMap Website.apply

  def userTypes (implicit request: Request[_]) = apply(request).map(_.userTypes).getOrElse(Services.config.userTypes)
  def userTypeDesc(ut:String)(implicit request: Request[_]) : String = apply(request).flatMap(_.userTypeDesc(ut)).getOrElse(ut)

  /** find or default */
  def get  (implicit request: Request[_]) : Website = apply getOrElse dflt
  def gets (implicit stok: StateOk) : Website = get(stok.request.get)

  def clean (host:String):Unit = { cache.remove(host) }

  def dflt = new Website(Wikis.rk.categories.head) //todo this is stupid - find better default

  /** @deprecated use PlayTools.getHost */
  def getHost (implicit request: RequestHeader) = PlayTools.getHost
}

