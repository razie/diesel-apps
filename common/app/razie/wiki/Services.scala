/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki

import controllers.{NoWikiAuthorization, WikiAuthorization}
import razie.db.RazMongo
import razie.wiki.model._
import razie.wiki.util.{AuthService, NoAuthService}

/** central point of customization - aka service registry
  *
  * todo use some proper injection pattern - this is not MT-safe
  *
  * right now this is setup in Global and different Module(s), upon startup
  */
object Services {
  var auth: AuthService[WikiUser] = NoAuthService
  var config: WikiConfig = new SampleConfig
  var wikiAuth: WikiAuthorization = new NoWikiAuthorization

  // this is only used for signed scripts - unsafe scripts are not ran here
  var runScriptImpl : (String, String, Option[WikiEntry], Option[WikiUser], Map[String, String], Boolean) => String =
    (script: String, lang:String, page: Option[WikiEntry], user: Option[WikiUser], query: Map[String, String], devMode:Boolean) =>
      "TODO customize scripster"

  var mkReactor : (String, List[Reactor], Option[WikiEntry]) => Reactor = { (realm, fallBack, we)=>
    new Reactor(realm, fallBack, we)
  }

  /** run the given script in the context of the given page and user as well as the query map */
  def runScript (s: String, lang:String, page: Option[WikiEntry], user: Option[WikiUser], query: Map[String, String], devMode:Boolean=false): String =
    runScriptImpl(s,lang, page,user,query,devMode)

  def noInitSample = {
    /** connect to your database, with your connection properties, clustered or not etc */
    import com.mongodb.casbah.MongoConnection
    RazMongo.setInstance {
      MongoConnection("") apply ("")
    }
  }

  /** is this website trusted? if not links will have a "exit" warning */
  var isSiteTrusted : String => Boolean = {s=>false }

  private var handlers: EventProcessor = new NoLater

  /** initialize the event processor */
  def initCqrs (al:EventProcessor) = {handlers = al}

  /** CQRS dispatcher */
  def ! (a: Any) = {handlers ! a}

  /** this is a generic event and/or task dispatcher - I simply ran out of names...
    *
    * some of the evens are audits, some are entity notifications that spread through the cluster
    *
    * some are just stuff to do later.
    */
  trait EventProcessor {
    /** execute work request later */
    def ! (a: Any)
  }
}

/** stub implementation - does nothing */
class NoLater extends Services.EventProcessor {
  def !(a: Any) {}
}

