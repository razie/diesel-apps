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
import razie.{cdebug, clog}
import razie.db.RazSalatContext._
import razie.db.{RMany, RazMongo}
import razie.wiki.dom.WikiDomain
import razie.wiki.parser.WikiParserT
import razie.wiki.util.DslProps
import razie.wiki.{Services, WikiConfig}

import scala.collection.mutable.ListBuffer

/** caching oft-used pages (categories etc)
  *
  * todo statics are bad, eh?
  * */
object WeCache {
  var loading=false
  var maxRecs = 500

  private var pre  = new ListBuffer[WikiEntry]()

  // special extra cache of categories per realm
  private var icats = new collection.mutable.HashMap[String,Map[String, WikiEntry]]()

  private var cache = new collection.mutable.HashMap[UWID,WikiEntry]()
  private var depys = new collection.mutable.HashMap[UWID,List[UWID]]()

  /** while starting up, we can't process wikis */
  def preload (we:WikiEntry) = {
    pre += we
    put(we, false)
  }

  /** lazy, at the end of starting up - process preloaded wikis */
  def load() = {
    if(!loading && pre.nonEmpty) {
      loading=true
      pre foreach update
      pre.clear()
    }
  }

  /** cache wiki */
  def put (we:WikiEntry, withDepy:Boolean=true) = synchronized {
    cache.put(we.uwid, we)
    if (we.category == "Category") {
      // categories sit also in the special icats
      val x:Map[String, WikiEntry] = icats.get(we.realm) getOrElse Map.empty
      icats.put(we.realm, x + (we.name -> we))
    }
    if(withDepy)  {
      we.included
      depys.put(we.uwid, we.depys)
    }
  }

  /** update cached wiki and recalculate dependencies */
  def update (we:WikiEntry): Unit = synchronized {
    we.sections
    val more = depys.get(we.uwid).toList.flatten
    cache.put(we.uwid, we)
    depys.put(we.uwid, we.depys)
    more.foreach(u=>update(u.page.get))
  }

  def get (realm:String, cat:String, name:String) = synchronized {
    load()
    cache.find(t=> t._2.realm == realm && t._2.category == cat && t._2.name == name)
  }

  def list (realm:String, cat:String) = synchronized {
    load()
    cache.filter(t=> t._2.realm == realm && t._2.category == cat).values.toList
  }

  def cats (realm:String) = synchronized {
    load()
    icats.getOrElse(realm, Map.empty)
  }

  def passthrough (ObjectId:String) = synchronized {
    load()
//    icats.getOrElse(realm, Map.empty)
  }

}

/** a wiki instance. corresponds to one reactor/realm
  *
  * has a list of fallbacks / mixins
  * */
class WikiInst (val realm:String, val fallBacks:List[WikiInst]) {
  /** this is the actual parser to use - combine your own and set it here in Global */
  def mkParser : WikiParserT = new WikiParserCls(realm)

  class WikiParserCls(val realm:String) extends WikiParserT // default simple parser

  val index  : WikiIndex  = new WikiIndex (realm, fallBacks.map(_.index))

  val mixins = new Mixins[WikiInst](fallBacks)

  val REALM = "realm" -> realm

  /** cache of categories - updated by the WikiIndex */
  {
    RMany[WikiEntry](REALM, "category" -> "Category") foreach (WeCache.preload _)
  }

  def cats = WeCache.cats(realm)

  /** cache of tags - updated by the WikiIndex */
  lazy val tags = new collection.mutable.HashMap[String,WikiEntry]() ++
    (RMany[WikiEntry](REALM, "category" -> "Tag") map (w=>(w.name,w))).toList

  def weTable(cat: String) = Wikis.TABLE_NAMES.get(cat).map(x=>RazMongo(x)).getOrElse(if (Wikis.PERSISTED contains cat) RazMongo("we"+cat) else table)
  def weTables(cat: String) = Wikis.TABLE_NAMES.getOrElse(cat, if (Wikis.PERSISTED contains cat) ("we"+cat) else Wikis.TABLE_NAME)
  def table = RazMongo(Wikis.TABLE_NAME)

  def foreach (f:DBObject => Unit) {table find(Map(REALM)) foreach f} // todo test

  def count = {table.find(Map(REALM)).count}

  // ================== methods from Wikis

  /** EXPENSIVE - find pages with category */
  def pages(category: String) : Iterator[WikiEntry] =
    if("*" == category)
      weTable(category).find(Map(REALM)) map (grater[WikiEntry].asObject(_))
    else
      weTable(category).find(Map(REALM, "category" -> category)) map (grater[WikiEntry].asObject(_))

  /** find pages with category */
  def pageNames(category: String) =
    weTable(category).find(Map(REALM, "category" -> category)) map (_.apply("name").toString)

  /** find pages with category */
  def pageLabels(category: String) =
    table.find(Map(REALM, "category" -> category)) map (_.apply("label").toString)

  // TODO optimize - cache labels... they can be changed by the page itself too...
  def label(wid: WID):String = /*wid.page map (_.label) orElse*/
    index.label(wid.name) orElse (ifind(wid) flatMap (_.getAs[String]("label"))) getOrElse wid.name

  def label(wid: UWID):String = /*wid.page map (_.label) orElse*/
    wid.wid.map(x=>label(x)).getOrElse(wid.nameOrId)

  /** get label from category definiition */
  def labelFor(wid: WID, action: String) =
    category(wid.cat) flatMap (_.contentProps.get("label." + action))

  import play.api.cache._
  import play.api.Play.current

  // this can't be further optimized - it SHOULD lookup the storage, to refresh stuff as well
  private def ifind(wid: WID) = {
    wid.parent.map {p=>
      // todo could simplify onle name unique in parent, no cat needed?
      if(wid.cat.isEmpty)
        weTable(wid.cat).findOne(Map(REALM, "name" -> wid.name, "parent" -> p))
      else
        weTable(wid.cat).findOne(Map(REALM, "category" -> wid.cat, "name" -> wid.name, "parent" -> p))
    } getOrElse {
      if (Services.config.cacheDb) {
        Cache.getAs[DBObject](wid.wpath+".db").orElse {
          clog << "WIKI_CACHED DB-" + wid.wpath
          val n = weTable(wid.cat).findOne(Map(REALM, "category" -> wid.cat, "name" -> Wikis.formatName(wid.name)))
          n.filter(x => !Wikis.PERSISTED.contains(wid.cat)).map {
            // only for main wikis with no parents
            // todo can refine this logic further
            Cache.set(wid.wpath + ".db", _, 300) // 10 minutes
          }
          n
        }
      } else {
        weTable(wid.cat).findOne(Map(REALM, "category" -> wid.cat, "name" -> Wikis.formatName(wid.name)))
      }
    }
  }

  // TODO find by ID is bad, no - how to make it work across wikis ?
  def findById(id: String) = find(new ObjectId(id))
  // TODO optimize
  def find(id: ObjectId) =
    (
      table.findOne(Map("_id" -> id)) orElse (
        Wikis.PERSISTED.find {cat=>
          weTable(cat).findOne(Map("_id" -> id)).isDefined
        } flatMap {s:String=>
          weTable(s).findOne(Map("_id" -> id))})
      ) map (grater[WikiEntry].asObject(_)
    )

  def findById(cat:String, id: String):Option[WikiEntry] = findById(cat, new ObjectId(id))

  def findById(cat:String, id: ObjectId): Option[WikiEntry] =
    weTable(cat).findOne(Map("_id" -> id)) map (grater[WikiEntry].asObject(_)) // todo not restricted by realm

  def find(wid: WID): Option[WikiEntry] =
    if(wid.cat.isEmpty && wid.parent.isEmpty) {
      // some categories are allowed without cat if there's just one of them by name
      val wl = findAny(Wikis.formatName(wid.name)).toList
      val wlf = wl.filter(we=>Array("Blog", "Post").contains(we.wid.cat))
      if(wlf.size == 1) Some(wlf.head) // prefer blog/post over other categories
      else if(wl.size == 1) Some(wl.head)
      else {
        val wll = wl.filter(_.wid.getRealm == "rk")
        if(wll.size == 1) Some(wll.head)
        else None // todo it was like this: // don't want to randomly find what others define with same name...
        //todo if someone else defines a blog/forum with same name, it will not find mine anymore - so MUST use REALMS
      }
    } else {
    // optimized for Categories
      if(wid.cat == "Category")
        category(wid.name)
      else
//        ifind(wid) map (grater[WikiEntry].asObject(_)) orElse fallback.flatMap(_.find(wid))
      ifind(wid) orElse mixins.first(_.ifind(wid)) map (grater[WikiEntry].asObject(_))
    }

  def find(uwid: UWID): Option[WikiEntry] = findById(uwid.cat, uwid.id)

  def find(category: String, name: String): Option[WikiEntry] = find(WID(category, name))

  /** find any topic with name - will look in PERSISTED tables as well until at least one found */
  def findAny(name: String) : Iterator[WikiEntry] = {
    val w1 = table.find(Map(REALM, "name" -> name)) map (grater[WikiEntry].asObject(_))
    val res = if(w1.hasNext) w1
    else {
      var found:Option[DBObject]=None
      Wikis.PERSISTED.find {cat=>
        if(found.isEmpty)
          found= weTable(cat).findOne(Map(REALM, "name" -> name))
        found.isDefined
      }
      found.map(grater[WikiEntry].asObject(_)).toIterator
    }
    if(res.hasNext || fallBacks.isEmpty) res
    else mixins.firstThat(_.findAny(name))(_.hasNext)(res)
  }

  def findAnyOne(name: String) =
    table.findOne(Map(REALM, "name" -> name)) map (grater[WikiEntry].asObject(_))

  def categories = cats.values
  def category(cat: String) : Option[WikiEntry] = cats.get(cat).orElse(mixins.first(_.category(cat)))

  def refreshCat (we:WikiEntry): Unit = {
    WeCache.put(we)
// scan all other realms that may [[include:: this category
//    Reactors.reactors.values.filter(_.realm != realm).map{r=>
//      if(r.wiki.cats contains we.wid.name)
//        r.wiki.cats.remove(we.wid.name)
//    }
  }

  final val VISIBILITY = "visibility"

  /** can override in cat, fallback to reactor, fallback to what's here */
  def visibilityFor(cat: String, prop:String = VISIBILITY): Seq[String] =
    cats.get(cat).flatMap(_.contentProps.get(prop)).orElse(
      WikiReactors(realm).props.prop(prop)
    ).getOrElse(
      "Public,Private"
    ).split(",").toSeq

  /** see if any of the tags of a page are nav tags */
  def navTagFor(pageTags: Seq[String]) = pageTags.map(tags.get).find(op=>op.isDefined && op.get.contentProps.contains("navTag"))

  /** look for and apply any formatting templates
    *
    * Formatting templates are used to re-format pages for display. They're used usually to decorate with functionality
    * like menus, buttons etc
    *
    * @param wid
    * @param content
    * @param which one of html|json
    * @return the new content, with the templates applied
    */
  def applyTemplates (wid:WID, content:String, which:String) = {
    val wpath=wid.wpath
    var res = content
      res = category(wid.cat).flatMap(_.section("template", which)).fold(content) { sec =>
        sec.signature match {
          case "interpolated" => {
            content
          }
          case "uppercase" => {
            sec.content.
              replaceAllLiterally("NAME", wid.name).
              replaceAllLiterally("REALM", wid.getRealm).
              replaceAllLiterally("WPATH", wpath).
              replaceAllLiterally("CONTENT", content)
            // todo important to have COTENT at the end so it doenst replace inside it
          }
          case _ => {
            sec.content.
              replaceAllLiterally("{{$$name}}", wid.name).
              replaceAllLiterally("{{$$realm}}", wid.getRealm).
              replaceAllLiterally("{{$$wpath}}", wpath).
              replaceAllLiterally("{{$$content}}", content)
            // it is important to have $$content at the end so it doenst replace inside it
            //todo use the ast folding with T_TEMPLATE
          }
        }
      }
    res
  }
}

