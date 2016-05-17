/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki

import controllers.{NoWikiAuthorization, WikiAuthorization}
import razie.db.{RazMongo, UpgradeDb}
import razie.wiki.model._
import razie.wiki.util.{NoAuthService, AuthService}
import razie.wiki.admin.{AuditService, NoAuditService}

/** central point of customization - aka service registry
  *
  * todo use some proper injection pattern - this is not MT-safe
  *
  * right now this is setup in Global, upon startup
  */
object Services {
  var auth: AuthService[WikiUser] = NoAuthService
  var audit: AuditService = NoAuditService
  var config: WikiConfig = null
  var wikiAuth: WikiAuthorization = new NoWikiAuthorization

  // called when configuration is reloaded - use them to refresh your caches
  val configCallbacks = new collection.mutable.ListBuffer[() => Unit]()

  // this is only used for signed scripts - unsafe scripts are not ran here
  var runScriptImpl : (String, String, Option[WikiEntry], Option[WikiUser], Map[String, String], Boolean) => String =
    (script: String, lang:String, page: Option[WikiEntry], user: Option[WikiUser], query: Map[String, String], devMode:Boolean) =>
      "TODO customize scripster"

  var mkReactor : (String, List[Reactor], Option[WikiEntry]) => Reactor = { (realm, fallBack, we)=>
    new Reactor(realm, fallBack, we)
  }

  private var alli: Alligator = NoAlligator

  /** add a callback to be called when the configuration is refreshed - use it to refresh your own configuration and/or caches */
  def configCallback (f:() => Unit) = {
    configCallbacks append f
  }

  /** run the given script in the context of the given page and user as well as the query map */
  def runScript (s: String, lang:String, page: Option[WikiEntry], user: Option[WikiUser], query: Map[String, String], devMode:Boolean=false): String =
    runScriptImpl(s,lang, page,user,query,devMode)

  def initAlli (al:Alligator) = {alli = al}

  def noInitSample = {
    /** connect to your database, with your connection properties, clustered or not etc */
    import com.mongodb.casbah.{MongoConnection, MongoDB}
    RazMongo.setInstance {
      MongoConnection("") apply ("")
    }
  }

  /** execute work request later */
  def !  (a: Any) = {alli ! a}

  /** execute work request sync */
  def !? (a: Any) = {alli !? a}

  /** is this website trusted? if not links will have a "exit" warning */
  var isSiteTrusted : String => Boolean = {s=>false }
}

/** this is a generic event and/or task dispatcher - I simply ran out of names...
  *
  * some of the evens are audits, some are entity notifications that spread through the cluster
  *
  * some are just stuff to do later.
  */
trait Alligator {
  /** execute work request later */
  def ! (a: Any)

  /** execute work request sync */
  def !?(a: Any)
}

/** stub implementation - does nothing */
object NoAlligator extends Alligator {
  def !(a: Any) {}
  def !?(a: Any) {}
}

