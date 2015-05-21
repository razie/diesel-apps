/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki

import razie.db.{RazMongo, UpgradeDb}
import razie.wiki.model._
import razie.wiki.util.{NoAuthService, AuthService}
import razie.wiki.admin.{AuditService, NoAuditService}

/** central point of customization - aka service registry */
// todo use some proper pattern - yeah, I hate myself...
object Services {
  var auth: AuthService[WikiUser] = NoAuthService
  var audit: AuditService = NoAuditService
  var config: WikiConfig = null

  // called when configuration is reloaded - use them to refresh your caches
  val configCallbacks = new collection.mutable.ListBuffer[() => Unit]()

  /** add a callback to be called when the configuration is refreshed - use it to refresh your own configuration and/or caches */
  def configCallback (f:() => Unit) = {
    configCallbacks append f
  }

  /** run the given script in the context of the given page and user as well as the query map */
  def runScript (s: String, page: Option[WikiEntry], user: Option[WikiUser], query: Map[String, String], devMode:Boolean=false): String =
    runScriptImpl(s,page,user,query,devMode)

  var runScriptImpl : (String, Option[WikiEntry], Option[WikiUser], Map[String, String], Boolean) => String =
    (s: String, page: Option[WikiEntry], user: Option[WikiUser], query: Map[String, String], devMode:Boolean) =>
    "TODO customize scripster"

  var mkReactor : (String, Option[Reactor], Option[WikiEntry]) => Reactor = { (realm, fallBack, we)=>
    new Reactor(realm, fallBack, we)
  }

  var alli: Alligator = NoAlligator

  def noInitSample = {
    /** connect to your database, with your connection properties, clustered or not etc */
    import com.mongodb.casbah.{MongoConnection, MongoDB}
    RazMongo.setInstance {
      MongoConnection("") apply ("")
    }
  }

  /** is this website trusted? if not links will have a "exit" warning */
  var isSiteTrusted : String => Boolean = {s=>false }

}

/** don't ask... still decoupling this */
trait Alligator {
  /** execute work request later */
  def ! (a: Any)

  /** execute work request now */
  def !?(a: Any)
}

object NoAlligator extends Alligator {
  def !(a: Any) {}
  def !?(a: Any) {}
}

