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

import scala.collection.mutable.HashMap

/** the index is (name, WID, ID) */
class WikiIndex (val realm:String, val fallBack : Option[WikiIndex]) {
  case class PEntry(ilinks: List[ILink])
  private val parsed = scala.collection.mutable.Map[ObjectId, PEntry]()

  private val lower = HashMap[String, String]() // for lowercase
  private val labels = HashMap[String, String]() // quick label lookup

  private lazy val actualIndex = {
    val t = new TripleIdx[String, WID, ObjectId]()
    // load it the first time
    Wikis(realm).foreach { db =>
      val w = WID(
        db.as[String]("category"),
        db.as[String]("name"),
        if (db.containsField("parent")) Some(db.as[ObjectId]("parent")) else None,
        None).r(db.as[String]("realm"))
      t.put(w.name, w, db.as[ObjectId]("_id"))
      lower.put(w.name.toLowerCase(), w.name)
      labels.put(w.name, db.as[String]("label"))
    }
    t
  }

  /** the index is (name, WID, ID) */
  def withIndex[A](f: TripleIdx[String, WID, ObjectId] => A) = {
    synchronized {
      // todo man i'm lazy - won't work for more than 3 levels of defaults
      fallBack.flatMap(_.fallBack).map(_.actualIndex) // load the lazy thing -
      fallBack.map(_.actualIndex) // load the lazy thing
      f(actualIndex)
    }
  }

  /** update an entry - call AFTER the we is persisted */
  private def up(we: WikiEntry) {
    parsed.put(we._id, PEntry(we.ilinks))
    if (we.wid.cat == "Category") {
      Wikis(realm).refreshCat(we)
      WikiDomain(realm).resetDom
    } else if (we.wid.cat == "Tag") {
      Wikis(realm).tags.put(we.wid.name, we)
    }
  }

  def graph(oid: ObjectId) = synchronized {
    parsed.getOrElseUpdate(oid, PEntry(Wikis(realm).find(oid).toList.flatMap(_.ilinks)))
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
//    }
    up(newVer)
  }

  def create(we: WikiEntry) = withIndex { idx =>
    idx.put(we.name, we.wid, we._id)
    lower.put(we.name.toLowerCase(), we.name)
    labels.put(we.name, we.label)
    up(we)
  }

  def delete(we: WikiEntry) = withIndex { idx =>
    idx.remove2(we.name, we.wid)
    lower.remove(we.name.toLowerCase())
    labels.remove(we.name)
    parsed.remove(we._id)
  }

  def containsName(name: String) = withIndex { idx =>
    idx.idx.contains(name) || fallBack.exists(_.actualIndex.idx.contains(name))
  }

  def containsLower(name: String) = withIndex { idx =>
    lower.contains(name) || fallBack.exists(_.lower.contains(name))
  }

  // TODO stupidest name this month
  def getForLower(name: String) = withIndex { idx =>
    lower.get(name) orElse fallBack.flatMap(_.lower.get(name))
  }

  // TODO what's the best way to optimize this - at least track stats and optimize when painful
  def getOptions(str: String) : List[String] = withIndex { idx =>
    GlobalData.wikiOptions += 1
    lower.filterKeys(_.startsWith(str)).map(_._2).take(20).toList ::: fallBack.map(_.getOptions(str)).getOrElse(Nil)
  }

  /** get list of wids that matches the name */
  def getWids (name:String) : List[WID] = withIndex {idx=>
    val wids = idx.get1k(name)
    if(wids.isEmpty) fallBack.toList.flatMap(_.getWids(name))
    else wids
  }

  def label(name: String) = withIndex { idx =>
    labels.get(name) orElse fallBack.flatMap(_.labels.get(name))
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
  /** the index is (name, WID, ID) */
  def withIndex[A](realm:String=Wikis.DFLT)(f: TripleIdx[String, WID, ObjectId] => A) =
    Wikis(realm).index.withIndex(f)
}
