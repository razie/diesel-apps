/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.model

import com.mongodb.casbah.Imports._
import razie.base.data.TripleIdx
import razie.wiki.admin.GlobalData
import razie.wiki.dom.WikiDomain

import scala.collection.mutable.{ListBuffer, HashMap}

/** generic instance mixin - wiki domains and indexes accept mixins */
class Mixins[A <: {def mixins:Mixins[A]}] (val l:List[A]) {
  val flatten = {
    var seen = new ListBuffer[A]()
    def see (a:A): Unit = {
      if(! (seen contains a)) {
        seen.append(a)
        a.mixins.l.foreach (see)
      }
    }
    l.foreach (see)
    seen.toList
  }

  def first[B] (f:A=>List[B]) : List[B] = {
    l.map(f).collectFirst {case x if !x.isEmpty => x }.toList.flatten
  }

  def first[B] (f:A=>Option[B]) : Option[B] = {
    l.map(f).collectFirst {case x if !x.isEmpty => x }.flatten
  }

  def firstThat[B] (f:A=>B)(cond:B=>Boolean)(unit: =>B) : B = {
    l.map(f).collectFirst {case x if cond(x) => x }.getOrElse(unit)
  }
}

/** the index is (name, WID, ID) */
class WikiIndex (val realm:String, val fallBacks : List[WikiIndex]) {
  case class PEntry(ilinks: List[ILink])
  private val parsed = scala.collection.mutable.Map[ObjectId, PEntry]()

  private val lower = HashMap[String, String]() // for lowercase
  private val labels = HashMap[String, String]() // quick label lookup

  val usedTags = HashMap[String, String]() // tags used in all pages

  val mixins = new Mixins[WikiIndex](fallBacks)

  /** the index is (name, (WID, ID)*)* with multiple WIDs per name */
  private lazy val actualIndex : TripleIdx[String, WID, ObjectId] = load

  /** load it the first time */
  private def load = {
    // todo should this be sync'd ?
    /** the index is (name, (WID, ID)*)* with multiple WIDs per name */
    val t = new TripleIdx[String, WID, ObjectId]()

    // avoid parsing each
    Wikis(realm).foreach { db =>
      val iw = WID(
        db.as[String]("category"),
        db.as[String]("name"),
        if (db.containsField("parent")) Some(db.as[ObjectId]("parent")) else None,
        None
      ).r(db.as[String]("realm"))

      // RK is just one other realm
      val w = if(iw.realm.isDefined) iw else iw.r(Wikis.RK)
      t.put(w.name, w, db.as[ObjectId]("_id"))
      lower.put(w.name.toLowerCase(), w.name)
      labels.put(w.name, db.as[String]("label"))

      if (db.containsField("tags")) {
        val tags2 = db.as[Seq[String]]("tags")
        if(tags2 != null) {
          tags2.foreach { t =>
            usedTags.put(t, "")
          }
        }
      }
    }
    t
  }

  /** the index is (name, WID, ID) */
  def withIndex[A](f: TripleIdx[String, WID, ObjectId] => A) = {

    synchronized {
      // todo man i'm lazy - won't work for more than 3 levels of defaults
      fallBacks.flatMap(_.fallBacks).map(_.actualIndex) // load the lazy thing -
      fallBacks.map(_.actualIndex) // load the lazy thing
      f(actualIndex)
    }
  }

  /** update an entry - call AFTER the we is persisted */
  private def up(we: WikiEntry) {
    if (we.wid.cat == "Category") {
      Wikis(realm).refreshCat(we)
      WikiDomain(realm).resetDom
    } else if (we.wid.cat == "Tag") {
      Wikis(realm).tags.put(we.wid.name, we)
    }
    parsed.put(we._id, PEntry(we.ilinks))
  }

  def graph(oid: ObjectId) = synchronized {
    parsed.getOrElseUpdate(oid, PEntry(Wikis(realm).find(oid).toList.flatMap(_.ilinks)))
  }

  /** update from remote - in case there is no old version */
  def update(neww: WID, oldw: WID) = withIndex { idx =>
    //    if (oldVer.category != newVer.category || oldVer.name != newVer.name || oldVer.realm != newVer.realm) {

    // doing it all the time because these WIDs cache the old version of the page so they need refreshing

    idx.remove2(oldw.name, oldw)
    oldw.uwid.foreach(oldUwid=>
      idx.put(neww.name, neww, oldUwid.id)
    )
    lower.remove(oldw.name.toLowerCase)
    lower.put(neww.name.toLowerCase(), neww.name)
    labels.remove(oldw.name)
    neww.page.foreach(newPage=>
      labels.put(neww.name, newPage.label)
    )

    //todo optimize - if changed
    neww.page.foreach(_.tags.foreach { t =>
      usedTags.put(t, "")
    })

    neww.page.foreach(up)
  }

  def update(oldVer: WikiEntry, newVer: WikiEntry) = withIndex { idx =>
//    if (oldVer.category != newVer.category || oldVer.name != newVer.name || oldVer.realm != newVer.realm) {

    // doing it all the time because these WIDs cache the old version of the page so they need refreshing

    idx.remove2(oldVer.name, oldVer.wid)
    idx.put(newVer.name, newVer.wid, oldVer._id)
    lower.remove(oldVer.name)
    lower.put(newVer.name.toLowerCase(), newVer.name)
    labels.remove(oldVer.name)
    labels.put(newVer.name, newVer.label)

    //todo optimize - if changed
    newVer.tags.foreach { t =>
      usedTags.put(t, "")
    }
//    }
    up(newVer)
  }

  def create(we: WikiEntry) = withIndex { idx =>
    idx.put(we.name, we.wid, we._id)
    lower.put(we.name.toLowerCase(), we.name)
    labels.put(we.name, we.label)
    up(we)
  }

  def delete(wid: WID, id:ObjectId) = withIndex { idx =>
    idx.remove2(wid.name, wid)
    lower.remove(wid.name.toLowerCase())
    labels.remove(wid.name)
    parsed.remove(id)
  }

  def containsName(name: String) : Boolean = withIndex { idx =>
//    idx.idx.contains(name) || fallBack.exists(_.actualIndex.idx.contains(name))
    idx.idx.contains(name) || fallBacks.exists(_.containsName(name))
  }

  def containsLower(name: String)  : Boolean = withIndex { idx =>
//    lower.contains(name) || fallBack.exists(_.lower.contains(name))
    lower.contains(name) || fallBacks.exists(_.containsLower(name))
  }

  // TODO stupidest name this month
  def getForLower(name: String) : Option[String] = withIndex { idx =>
//    lower.get(name) orElse fallBack.flatMap(_.lower.get(name))
    lower.get(name) orElse mixins.first(_.getForLower(name))
  }

  // TODO what's the best way to optimize this - at least track stats and optimize when painful
  // cat can be a comma-sep-list
  def getOptions(cat:String, str: String, cnt:Int) : List[String] = withIndex { idx =>
    GlobalData.wikiOptions += 1
    val temp = lower.filterKeys(_.contains(str))
    val CATS = cat.split(", ")
    val x = if(cat != "") temp.filter(x=>idx.get1k(x._2).exists(x=>CATS.contains(x.cat))) else temp
    val y = x.take(cnt)
    y.map(_._2).toList //::: fallBack.map(_.getOptions(str, cnt-temp.size)).getOrElse(Nil)
  }

  /** get list of wids that matches the name */
  def getWids (name:String) : List[WID] = withIndex {idx=>
    val wids = idx.get1k(name)
    if(wids.isEmpty) mixins.first(_.getWids(name)) // first or all ???
    else wids
  }

  def label(name: String) : Option[String] = withIndex { idx =>
//    labels.get(name) orElse fallBack.flatMap(_.labels.get(name))
    labels.get(name) orElse mixins.first(_.label(name))
  }

  /** returns a random WID from index - keep calling until isDefined */
  def random: Option[WID] = withIndex { idx =>
    val k = labels.keySet.toSeq
    val r = (math.random * k.size).toInt
    if (k.isDefinedAt(r)) {
      idx.get1k(k(r)).headOption
    } else None
  }
}
/** the index is (name, WID, ID) */
object WikiIndex {
  def init() = {}

  /** the index is (name, WID, ID) */
  def withIndex[A](realm:String=Wikis.DFLT)(f: TripleIdx[String, WID, ObjectId] => A) =
    Wikis(realm).index.withIndex(f)

  WikiObservers mini {
    case ev@WikiEvent(action, "WikiEntry", _, entity, _, _, _) => {
      val wid = WID.fromPath(ev.id).get
      val index = Wikis(wid.getRealm).index
      val oldWid = ev.oldId.flatMap(WID.fromPath)

      action match {
        case WikiAudit.UPD_RENAME =>
          index.update(wid, oldWid.get)
        case WikiAudit.DELETE_WIKI =>
          if (wid.shouldIndex) index.delete(wid, new ObjectId(ev.oldId.mkString))
        case _ => {
          val swe = entity.asInstanceOf[Option[WikiEntry]]
          if (swe.exists(_.wid.shouldIndex)) {
            val we = swe.get
            if(entity.isDefined && ev.oldEntity.isDefined)
              Wikis(we.realm).index.update(ev.oldEntity.get.asInstanceOf[WikiEntry], we)
            else
              Wikis(we.realm).index.update(wid, oldWid getOrElse wid) // old wid may have changed
          }
        }
      }
    }
  }

}
