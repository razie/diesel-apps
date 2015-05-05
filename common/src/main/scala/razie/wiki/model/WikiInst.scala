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
import play.api.mvc.Request
import razie.db.{RazMongo, RMany}
import razie.db.RazSalatContext._
import razie.wiki.dom.WikiDomain
import razie.wiki.parser.{nWikiParser, WikiParserT}
import razie.wiki.util.PlayTools
import razie.wiki.{Services, WikiConfig}

/** reactor management */
object Reactors {
  // reserved reactors
  final val DFLT = WikiConfig.RK       // original, still decoupling code
  final val WIKI = "wiki"              // main reactor
  final val NOTES = WikiConfig.NOTES

  final val ALIASES = Map ("www" -> "wiki")

  /** lower case index - reactor names are insensitive */
  val lowerCase = new collection.mutable.HashMap[String,String]()

  //todo - scale... now all realms currently loaded in this node
  lazy val reactors = {
    val res = new collection.mutable.HashMap[String,Reactor]()

    // load reserved reactors: rk and wiki first
    val rk = Services.mkReactor(DFLT, None)
    res.put (DFLT, rk)
    lowerCase.put(DFLT, DFLT)

    res.put (NOTES, Services.mkReactor(NOTES, None))
    lowerCase.put(NOTES, NOTES)

    val wiki = Services.mkReactor(WIKI, Some(rk))
    res.put (WIKI, wiki)
    lowerCase.put(WIKI, WIKI)

    // load all other reactors
    rk.wiki.weTable("WikiEntry").find(Map("category" -> "Reactor")).map(grater[WikiEntry].asObject(_)).filter(x=>
      !(Array(DFLT, NOTES, WIKI) contains x.name)).foreach {w=>
      res.put (w.name, Services.mkReactor(w.name, Some(wiki)))
      lowerCase.put(w.name.toLowerCase, w.name)
    }

    res
  }

  def findWikiEntry(r:String) =
    rk.wiki.weTable("WikiEntry").findOne(Map("category" -> "Reactor", "name" -> r)).map(grater[WikiEntry].asObject(_))

  def reload(r:String): Unit = {
    findWikiEntry(r).foreach{w=>
      reactors.put (w.name, Services.mkReactor(w.name, Some(wiki)))
      lowerCase.put(w.name.toLowerCase, w.name)
    }
  }

  def rk = reactors(DFLT)
  def wiki = reactors(WIKI)

  def add (realm:String): Reactor = {
    assert(! reactors.contains(realm), "Sorry, SITE_ERR: Reactor already active ???")
    val r = Services.mkReactor(realm, reactors.get(WIKI))
    reactors.put(realm, r)
    lowerCase.put(realm.toLowerCase, realm)
    r
  }

  def contains (realm:String) = reactors.contains(realm)

  def apply (realm:String = Wikis.RK) = reactors.getOrElse(realm, rk) // using RK as a fallback

}

/** the basic element of reaction: an app or module... also a wiki instance */
class Reactor (val realm:String, val fallback:Option[Reactor] = None) {
  val wiki   : WikiInst   = new WikiInst(realm, fallback.map(_.wiki))
  val domain : WikiDomain = new WikiDomain (realm, wiki)

  /** Admin:UserHome if user or Admin:Home or Reactor:realm if nothing else is defined */
  def mainPage(au:Option[WikiUser]) = {
    val p = if(au.isDefined) WID("Admin", "UserHome").r(realm).page.map(_.wid) else None
    p orElse WID("Admin", "Home").r(realm).page.map(_.wid) getOrElse WID("Reactor", realm).r(realm)
  }
}

/** a wiki */
class WikiInst (val realm:String, val fallback:Option[WikiInst]) {
  /** this is the actual parser to use - combine your own and set it here in Global */
  def mkParser : WikiParserT = new WikiParserCls(realm)

  class WikiParserCls(val realm:String) extends WikiParserT // default simple parser

  val index  : WikiIndex  = new WikiIndex (realm, fallback.map(_.index))
  
  val REALM = "realm" -> realm

  /** cache of categories - updated by the WikiIndex */
  lazy val cats = new collection.mutable.HashMap[String,WikiEntry]() ++
    (RMany[WikiEntry](REALM, "category" -> "Category") map (w=>(w.name,w)))

  /** cache of tags - updated by the WikiIndex */
  lazy val tags = new collection.mutable.HashMap[String,WikiEntry]() ++
    (RMany[WikiEntry](REALM, "category" -> "Tag") map (w=>(w.name,w)))

  def weTable(cat: String) = Wikis.TABLE_NAMES.get(cat).map(x=>RazMongo(x)).getOrElse(if (Wikis.PERSISTED contains cat) RazMongo("we"+cat) else table)
  def weTables(cat: String) = Wikis.TABLE_NAMES.getOrElse(cat, if (Wikis.PERSISTED contains cat) ("we"+cat) else Wikis.TABLE_NAME)
  def table = RazMongo(Wikis.TABLE_NAME)

  def foreach (f:DBObject => Unit) {table find(Map(REALM)) foreach f} // todo test

  def count = {table.find(Map(REALM)).count}

  // ================== methods from Wikis

  def pages(category: String) =
    weTable(category).find(Map(REALM, "category" -> category)) map (grater[WikiEntry].asObject(_))

  def pageNames(category: String) =
    weTable(category).find(Map(REALM, "category" -> category)) map (_.apply("name").toString)

  def pageLabels(category: String) =
    table.find(Map(REALM, "category" -> category)) map (_.apply("label").toString)

  // TODO optimize - cache labels...
  def label(wid: WID):String = /*wid.page map (_.label) orElse*/
    index.label(wid.name) orElse (ifind(wid) flatMap (_.getAs[String]("label"))) getOrElse wid.name

  def label(wid: UWID):String = /*wid.page map (_.label) orElse*/
    wid.wid.map(x=>label(x)).getOrElse(wid.nameOrId)

  private def ifind(wid: WID) = {
    wid.parent.map {p=>
      weTable(wid.cat).findOne(Map(REALM, "category" -> wid.cat, "name" -> wid.name, "parent" -> p))
    } getOrElse {
        weTable(wid.cat).findOne(Map(REALM, "category" -> wid.cat, "name" -> Wikis.formatName(wid.name)))
    }
  }

  // TODO find by ID is bad, no - how to make it work across wikis ?
  def findById(id: String) = find(new ObjectId(id))
  // TODO optimize
  def find(id: ObjectId) =
    (table.findOne(Map("_id" -> id)) orElse (Wikis.PERSISTED.find {cat=>
      weTable(cat).findOne(Map("_id" -> id)).isDefined
    } flatMap {s:String=>weTable(s).findOne(Map("_id" -> id))})) map (grater[WikiEntry].asObject(_))

  def findById(cat:String, id: String):Option[WikiEntry] = findById(cat, new ObjectId(id))

  def findById(cat:String, id: ObjectId): Option[WikiEntry] =
    weTable(cat).findOne(Map("_id" -> id)) map (grater[WikiEntry].asObject(_)) // todo not restricted by realm

  def find(wid: WID): Option[WikiEntry] =
    if(wid.cat.isEmpty && wid.parent.isEmpty) {
      // some categories are allowed without cat if there's just one of them by name
      val wl = findAny(wid.name).filter(we=>Array("Blog", "Post").contains(we.wid.cat)).toList
      if(wl.size == 1) Some(wl.head)
      else {
        val wll = wl.filter(_.wid.getRealm == "rk")
        if(wll.size == 1) Some(wll.head)
        else None // don't want to randomly find what others define with same name...
        //todo if someone else defines a blog/forum with same name, it will not find mine anymore - so MUST use REALMS
      }
    } else
      ifind(wid) orElse fallback.flatMap(_.ifind(wid)) map (grater[WikiEntry].asObject(_))

  def find(uwid: UWID): Option[WikiEntry] = findById(uwid.cat, uwid.id)

  def find(category: String, name: String): Option[WikiEntry] = find(WID(category, name))

  /** find any topic with name - will look in PERSISTED tables as well until at least one found */
  def findAny(name: String) = {
    val w1 = table.find(Map(REALM, "name" -> name)) map (grater[WikiEntry].asObject(_))
    if(w1.hasNext) w1
    else {
      var found:Option[DBObject]=None
      Wikis.PERSISTED.find {cat=>
        if(found.isEmpty)
          found= weTable(cat).findOne(Map(REALM, "name" -> name))
        found.isDefined
      }
      found.map(grater[WikiEntry].asObject(_)).toIterator
    }
  }

  def findAnyOne(name: String) =
    table.findOne(Map(REALM, "name" -> name)) map (grater[WikiEntry].asObject(_))

  def categories = cats.values
  def category(cat: String) = cats.get(cat)
  def visibilityFor(cat: String): Seq[String] =
    cats.get(cat).flatMap(_.contentTags.get("visibility")).map(_.split(",").toSeq).getOrElse(Seq("Public"))

  /** see if any of the tags of a page are nav tags */
  def navTagFor(pageTags: Seq[String]) = pageTags.map(tags.get).find(op=>op.isDefined && op.get.contentTags.contains("navTag"))
}


