/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki

import java.io.FileInputStream
import java.util.Properties
import com.google.inject.Singleton
import play.api.mvc.Request
import razie.clog
import razie.wiki.util.PlayTools
import scala.collection.mutable

/**
 * configuration for the core wiki engine - some come from a property file and some from special admin pages
 *
 * create a property file rk.properties and include in classpath,
 * containing the properties below
 *
 * *****************************************************************
 * ************* you can access this via Services.config ***********
 * *****************************************************************
 */
abstract class WikiConfig {
  // load properties from system or from a file rk.properties
  val props = {
    val p = new Properties();
    if(System.getProperty("rk.properties") != null) {
      p.load(new FileInputStream(System.getProperty("rk.properties")))
    } else {
      clog << "================ E R R O R        rk.properties ==================\n" + p.toString
      clog << "you do not have a file rk.properties in the classpath, using defaults"
      p.put("rk.hostport", "localhost:9000")
      p.put("rk.safemode", "no")
      p.put("rk.noads", "true")
      p.put("rk.forcephone", "false")

    }
    clog << "================ rk.properties ==================\n" + p.toString
    p
  }

  final val rk = System.getProperty("rk.home", props.getProperty("rk.home"))
  final val hostport = props.getProperty("rk.hostport")
  final val node = props.getProperty("rk.node", hostport)//java.net.InetAddress.getLocalHost.getCanonicalHostName)
  final val safeMode = props.getProperty("rk.safemode")
  final val analytics = true; //props.getProperty("rk.analytics").toBoolean
  val noads = props.getProperty("rk.noads").toBoolean
  final val forcephone = props.getProperty("rk.forcephone").toBoolean

  final val mongodb = props.getProperty("rk.mongodb")
  final val mongohost = props.getProperty("rk.mongohost")
  final val mongouser = props.getProperty("rk.mongouser")
  final val mongopass = props.getProperty("rk.mongopass")

  /** when running on localhost, simulate this host */
  def simulateHost = props.getProperty("rk.simulateHost")

  def SUPPORT = sitecfg("support").getOrElse("support@racerkidz.com")

  def isLocalhost = "localhost:9000" == hostport

  //-------------- special admin/configuration pages

  /** if there is an external favorite canonical URL for this WPATH
    *
    * @param tags taken from a WikiEntry - using it plain to decouple code
    */
  def urlcanon(wpath: String, tags:Option[Seq[String]]) = {
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

  /** modify external sites mapped to external URLs - NOT when on localhost:9000 though */
  def urlmap(u: String) = {
    var res = u

    //todo this looks stupid - use startsWith like in canon above
    for (has <- config(URLMAP) if(! isLocalhost); site <- has) {
      res = res.replaceFirst("^%s".format(site._1), site._2)
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

  /** pre-configured user types */
  def userTypes(implicit request: Request[_])  = {
//    if(realm == WikiConfig.NOTES)
//       TODO configure these
//      List("Individual", "Organization")
//    else
      config(USERTYPES).toList.flatMap(_.keys.toList)
  }

  def config(s: String) = {
    if (xconfig.isEmpty) reloadUrlMap
    xconfig.get(s)
  }

  /** holds the entire wiki-based configuration, can reset to reload */
  protected val xconfig = mutable.Map[String, mutable.Map[String, String]]()

  final val URLCFG = "urlcfg"
  final val URLCANON = "urlcanon"
  final val URLMAP = "urlmap"
  final val URLFWD = "urlfwd"
  final val URLREWRITE = "urlrewrite"
  final val SITECFG = "sitecfg"
  final val TOPICRED = "topicred"
  final val USERTYPES = "usertypes"
  final val BANURLS = "banurls"

  /** override to implement the actual configuration loading */
  def reloadUrlMap: Unit
}

object WikiConfig {
  final val RK = "rk"
  final val NOTES = "notes"

  // parse a properties looking thing
  def parsep(content: String) = (content.split("\r\n")) filter (!_.startsWith("#")) map (_.split("=", 2)) filter (_.size == 2) map (x => (x(0), x(1)))
}

/** sample config - use for testing for instance. Before beginning a test, do Services.config = SampleConfig */
@Singleton()
class SampleConfig extends WikiConfig {
  def robotUserAgents = irobotUserAgents
  private var irobotUserAgents = List[String]()

  def trustedSites = itrustedSites
  private var itrustedSites = List[String]()

  def reservedNames = ireservedNames
  private var ireservedNames = List[String]()

  def reloadUrlMap {
    println("========================== SAMPLE RELOADING URL MAP ==============================")

    val props = ""

    for (c <- Array(SITECFG, TOPICRED, USERTYPES, BANURLS)) {
      val urlmaps = Some(Seq(props) flatMap WikiConfig.parsep)
      val xurlmap = (urlmaps.map(se => mutable.HashMap[String, String](se: _*)))
      println("========================== RELOADING URL MAP ==============================")
      xurlmap.map(xconfig.put(c, _))
    }

    for (u <- Seq(props) flatMap WikiConfig.parsep) {
      val RE = """([^.]+)\.(.*)""".r
      val RE(pre, prop) = u._1

      if (!xconfig.contains(pre))
        xconfig.put(pre, mutable.HashMap[String, String](prop -> u._2))
      else
        xconfig.get(pre).map(_.put(prop, u._2))
    }

    // todo should fire this event
    // Services ! new WikiConfigChanged

    irobotUserAgents = sitecfg("robots.useragents").toList.flatMap(s=>s.split("[;,]"))
    ireservedNames = sitecfg("reserved.names").toList.flatMap(s=>s.split("[;,]"))
    itrustedSites = sitecfg("trusted.sites").toList.flatMap(s=>s.split("[;,]"))
  }

}

