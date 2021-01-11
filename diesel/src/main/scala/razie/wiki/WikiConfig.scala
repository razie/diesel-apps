/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki

import com.google.inject.{Singleton, _}
import play.api.Configuration
import play.api.mvc.Request
import razie.hosting.Website
import razie.wiki.model._
import razie.wiki.util.PlayTools
import scala.collection.mutable
import scala.collection.mutable.HashMap

/** some statics
  *
  * todo this should be the config ovject, not Config
  */
object WikiConfig {
  @Inject() var playConfig:Configuration = null

  final val URLCANON = "urlcanon"
  final val URLREWRITE = "urlrewrite"
  final val URLFWD = "urlfwd"

  final val URLCFG = "urlcfg"
  final val SITECFG = "sitecfg"
  final val USERTYPES = "usertypes"
  final val BANURLS = "banurls"

  final val CFG_PAGES = Array(SITECFG, USERTYPES, BANURLS, URLCFG)

  final val RK = "rk"
  final val NOTES = "notes"

  /** get from play config - needed as in some places I can't load sitecfg before props are needed */
  def prop(name:String, dflt:String="") =
    if(playConfig.underlying.hasPath(name)) playConfig.underlying.getString(name) else dflt

  // parse a properties looking thing
  def parsep(content: String) =
    (content.split("\r*\n").map(_.trim)) filter (!_.startsWith("#")) map (_.split("=", 2)) filter (_.size == 2) map (x => (x(0).trim, x(1).trim))
}

/**
 * configuration for the core wiki engine - some comes from a property config file and some from special admin pages
 *
 * create a property file rk.properties and include in classpath,
 * containing the properties below
 *
 * *****************************************************************
 * ************* you can access this via Services.config ***********
 * *****************************************************************
  *
 */
abstract class WikiConfig {

  import WikiConfig._

  def pconfig = WikiConfig.playConfig.underlying

  /** get from play config - needed as in some places I can't load sitecfg before props are needed */
  def hasProp(name: String) = pconfig.hasPath(name)

  /** get from play config - needed as in some places I can't load sitecfg before props are needed */
  def prop(name: String, dflt: String = "") =
    if (pconfig.hasPath(name)) pconfig.getString(name) else dflt

  /** play config overwritten by sitecfg */
  def weprop(name: String, dflt: String = "") =
    sitecfg(name) orElse (if (pconfig.hasPath(name)) Some(pconfig.getString(name)) else None) getOrElse dflt

  final val home = prop("wiki.home")

  final val hostport    = prop("wiki.hostport")
  final val node        = prop("wiki.node", hostport)//java.net.InetAddress.getLocalHost.getCanonicalHostName)
  final val safeMode    = prop("wiki.safemode")
  final val analytics   = true; //props.getProperty("rk.analytics").toBoolean
  final val noads       = prop("wiki.noads", isLocalhost.toString).toBoolean
  final val forcephone  = prop("wiki.forcephone").toBoolean

  final val mongodb     = prop("wiki.mongodb")
  final val mongohost = prop("wiki.mongohost")
  final val mongoport = prop("wiki.mongoport", "27017").toInt
  final val mongouser = prop("wiki.mongouser")
  final val mongopass   = prop("wiki.mongopass")

  final val cacheWikis  = prop("wiki.cachewikis", "false").toBoolean
  final val cacheFormat = prop("wiki.cacheformat", "false").toBoolean
  final val cacheDb     = prop("wiki.cachedb", "false").toBoolean

  /** when running in localhost, keep quiet with the audit, so DB doesn't grow */
  final val localQuiet  = prop("wiki.localquiet", "false").toBoolean

  final val clusterMode = prop("wiki.cluster", "no")

  final val CONNECTED   = prop("wiki.connected", "connected")

  /** when running on localhost, simulate this host */
  def simulateHost      = prop("wiki.simulateHost")

  // todo is only used for auth on the support email when sending - to configure password per reactor
  /** support email */
  def SUPPORT = prop("wiki.supportEmail", "support@racerkidz.com")

  /** admin email */
  final val adminEmail = prop("wiki.adminEmail", "razie@razie.com")

  // when running on localhost, some functionality is enabled as opposed to running in cloud hosting mode
  def isLocalhost = hostport startsWith "localhost:"

  // not every localhost is dev mode
  def isDevMode = prop("wiki.devMode", "false").toBoolean

  //-------------- special admin/configuration pages

  /** if there is an external favorite canonical URL for this WPATH
    *
    * @param tags taken from a WikiEntry - using it plain to decouple code
    */
  def urlcanon(wpath: String, tags:Option[Seq[String]]) : Option[String] = {
    var res: Option[String] = None

    //todo make unit test for urlcfg based canon for enduroschool
    // todo optimize this somehow wiht just some lookups based on parent
    // look for configured entry points (parents) and topics
    config(URLCANON).flatMap(_.filterKeys(k=>wpath.startsWith(k)).headOption).map { site=>
      res = Some(wpath.replaceFirst("^%s".format(site._1), site._2))
    }

    //todo make unit test for tag based canon for enduroschool
    // todo optimize this somehow wiht just some lookups based on tags
    if(res.isEmpty) {
      // look for configured tags
     tags.flatMap(t=>config(URLCANON).flatMap(_.filterKeys(k=>t.contains(k)).headOption)).map { site=>
        res = Some(site._2 + "/" + wpath)
      }
    }

    res
  }

  /** modify external sites mapped to external URLs */
  def urlfwd(u: String) = config(URLFWD) flatMap (_.get(u))

  /** modify external sites mapped to external URLs */
  def urlrewrite(u: String) = config(URLREWRITE).flatMap (_.collectFirst {
      case (k,v) if u.matches(k) => u.replaceFirst(k, v)
    })

  /** generic site configuration */
  def sitecfg(parm: String) = config(SITECFG) flatMap (_.get(parm))

  /** @obsolete find the realm from the request parameters - hostport or forwarded-for or something */
  def xrealm(implicit request: Request[_]) = {
    if(request.host contains "localhost") WikiConfig.RK else {
      config("realm").map { m =>
        PlayTools.getHost match {
          case Some(x) if m contains x => m(x)
          case _ => WikiConfig.RK
        }
      } getOrElse WikiConfig.RK
    }
  }

  /** deprecated - use Website.usetTypes instead */
  def userTypes  = {
      config(USERTYPES).toList.flatMap(_.keys.toList)
  }

  def config(s: String) = {
    if (xconfig.isEmpty) reloadUrlMap
    xconfig.get(s)
  }

  /** holds the entire wiki-based configuration, can reset to reload */
  protected val xconfig = mutable.Map[String, mutable.Map[String, String]]()

  // preload these reactors - comma separated. make sure rk,notes,wiki are included in order
  def preload     = sitecfg("preload.reactors").getOrElse("rk,notes,wiki,ski,omniware")

  def getTheme (user:Option[WikiUser], request:Option[Request[_]]) = {
    // session settings override everything
    request.flatMap(_.session.get("css")) orElse (
      // then user
      user.flatMap(_.css)
      ) orElse (
      // or website settings
      request.flatMap(r=> Website(r)).flatMap(_.css)
      ) getOrElse ("light")
  }

  // no request available
  def isLight(au:Option[WikiUser], request:Option[Request[_]]=None) =
    getTheme (au, request) contains "light"
  // todo remove this - relies on statics
  def oldisLight(au:Option[WikiUser]) = isLight(au, None)

  private var ibadIps = Array("178.175.146.90")
  def badIps = ibadIps

  def robotUserAgents = irobotUserAgents
  protected var irobotUserAgents = List[String]()

  def trustedSites = itrustedSites
  protected var itrustedSites = List[String]()

  def reservedNames = ireservedNames
  protected var ireservedNames = List[String]()


  import WikiConfig.parsep

  // using sync here, although access is not... sometimes weird conflict
  // todo remove the object and reload atomically the Services.config
  def reloadUrlMap : Unit = {
    println("========================== RELOADING URL MAP ==============================")

    for (c <- Array(SITECFG, USERTYPES, BANURLS)) {
      val urlmaps = Some(Seq(Wikis.findSimple(WID("Admin", c)).map(_.content).getOrElse("")) flatMap parsep)
      val xurlmap = (urlmaps.map(se => HashMap[String, String](se: _*)))
      xurlmap.map(xconfig.put(c, _))
    }

    reload (Wikis.findSimple(WID("Admin", URLCFG)).map(_.content).getOrElse(""))
  }

  def reload (cfg:String) : Unit = synchronized {
    // concentrated all types in just one topic "urlcfg"
    for (u <- Seq(cfg) flatMap parsep) {
      val RE = """([^.]+)\.(.*)""".r
      val RE(pre, prop) = u._1

      if (!xconfig.contains(pre))
        xconfig.put(pre, HashMap[String, String](prop -> u._2))
      else
        xconfig.get(pre).map(_.put(prop, u._2))
    }

    xconfig.keys.foreach(x => {
      //      println("============= config topic: " + x + " size " + xconfig.get(x).size)
      //      xconfig.get(x).foreach(y => println(y.mkString("\n  ")))
    })

    Services ! new WikiConfigChanged("", this)

    irobotUserAgents = sitecfg("robots.useragents").toList.flatMap(s => s.split("[;,]"))
    ireservedNames = sitecfg("reserved.names").toList.flatMap(s => s.split("[;,]"))
    //     todo settle this - there are two places for configuring trusted sites
    itrustedSites = sitecfg("trusted.sites").toList.flatMap(s => s.split("[;,]"))
    ibadIps = sitecfg("badips").toList.flatMap(s => s.split("[;,]")).toArray

  }

}

/** sample config - use for testing for instance. Before beginning a test, do Services.config = SampleConfig */
@Singleton()
class SampleConfig extends WikiConfig {
  override def getTheme (user:Option[WikiUser], request:Option[Request[_]]) = "light"

  override def reloadUrlMap {
    reload( "" )
  }
}

