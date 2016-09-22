/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *   (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
import com.mongodb.casbah.Imports.ObjectId
import com.mongodb.casbah.MongoConnection

import play.api.Application
import play.api.mvc.WithFilters
import razie.db.RTable
import razie.db.RazMongo
import razie.wiki.Services
import razie.wiki.WikiConfig
import razie.wiki.model.Reactor
import razie.wiki.model.WID
import razie.wiki.model.WikiEntry
import razie.wiki.model.WikiInst
import razie.wiki.model.Wikis
import razie.wiki.parser.WikiParserT

/** customize some global handling errors */
object Global extends WithFilters {
  override def beforeStart(app: Application) {
    Services.config = new WikiConfig {
      override def reloadUrlMap = {}
    }

    /************** MONGO INIT *************/
    RazMongo.setInstance {
      lazy val conn = MongoConnection("localhost")

      /** the actual database - done this way to run upgrades before other code uses it */
      com.mongodb.casbah.commons.conversions.scala.RegisterConversionHelpers()
      com.mongodb.casbah.commons.conversions.scala.RegisterJodaTimeConversionHelpers()

      // authenticate
      val db = conn("wikireactor")
      if (!db.authenticate("user", "password")) {
        throw new Exception("Cannot authenticate. Login failed.")
      }
      db
    }

    // OPTIONAL - customize the reactor/wiki/parser
    Services.mkReactor = { (realm, fallBacks, we)=>
      new MyReactor(realm, fallBacks, we)
    }

    // create the default page
    if (!Wikis.find(WID.fromPath("Admin:Sample1").get).isDefined) {
      razie.db.tx { implicit txn =>
        new WikiEntry("Admin", "Sample1", "Sample1", "md", """
Congratulations - you made it work!

This is a sample first page. Try to create a new page: [[Admin:Sample1-1]].
""", new ObjectId()).create
      }

    }

  }
}

/** OPTIONAL: my own reactor - customize the customizables */
class MyReactor (realm:String, fallBacks:List[Reactor], we:Option[WikiEntry]) extends Reactor (realm, Nil, we) {

  /** my wiki - used to compose my own parser */
  class MyWikiInst (realm:String, fallBacks:List[WikiInst]) extends WikiInst(realm, fallBacks) {
    class MyWikiParserCls(val realm: String) extends WikiParserT {
    }

    override def mkParser = new MyWikiParserCls(realm)
  }

  override val wiki: WikiInst = new MyWikiInst(realm, Nil)
}


