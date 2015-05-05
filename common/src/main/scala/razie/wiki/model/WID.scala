/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.model

import com.mongodb.casbah.Imports._
import com.novus.salat._
import razie.db.RazSalatContext._
import razie.wiki.{WikiConfig, Services}
import razie.wiki.dom.WikiDomain
import model.CMDWID

/** a unique ID - it is less verbose than the WID - used in data modelling.
  *
  * also, having a wid means a page exists or existed
*/
case class UWID(cat: String, id:ObjectId, realm:Option[String]=None) {
  /** find the name and build a wid - possibly expensive for non-indexed topics */
  def findWid = {
    WikiIndex.withIndex(getRealm) { idx =>
      idx.find((_,_,x)=>x == id).map(_._2)
    } orElse Wikis(getRealm).findById(cat, id).map(_.wid)
  }
  /** force finding or building a surrogate wid */
  def wid = findWid orElse Some(WID(cat, id.toString)) // used in too many places to refactor properly
  def nameOrId = wid.map(_.name).getOrElse(id.toString)
  lazy val grated = grater[UWID].asDBObject(this)
  lazy val page = Wikis(getRealm).find(this)

  /** get the realm or the default */
  def getRealm = realm.getOrElse(Wikis.DFLT)

  /** withRealm - convienience builder */
  def r(r:String) = if(Wikis.DFLT == r) this else this.copy(realm=Some(r))
}

/** a wrapper for categories, since they can now have a realm */
case class CAT(cat: String, realm:Option[String]) { // don't give realm a None defaut, eh? see object.apply
  /** get the realm or the default */
  def getRealm = realm.getOrElse(Wikis.DFLT)
}

object CAT {
  def unapply (cat:String) : Option[CAT] = Some(apply(cat))
  def apply (cat:String) : CAT =
    if(cat.contains(".")) {
      val cs = cat.split("\\.")
      CAT(cs(1), Some(cs(0)))
    }
    else CAT(cat, None)
}

/** a wiki id, a pair of cat and name - can reference a wiki entry or a section of an entry
  *
  * format is parent/realm.cat:name#section
  *
  * assumption is that when we link between wikis, we'll have wiki/parent/cat:name#section -
  */
case class WID(cat: String, name: String, parent: Option[ObjectId] = None, section: Option[String] = None, realm:Option[String]=None) {
  override def toString = "[[" + wpath + "]]"

  lazy val grated     = grater[WID].asDBObject(this)
  lazy val findParent = parent flatMap (p => Wikis(getRealm).find(p))
  lazy val parentWid  = parent flatMap (p => WikiIndex.withIndex(getRealm) { index => index.find { case (a, b, c) => c == p }.map(_._2) }) orElse (findParent map(_.wid))

  /** find the page for this, if any - respects the NOCATS */
  lazy val page = {
    if(cat.isEmpty) findId flatMap Wikis(getRealm).find // special for NOCATS
    else Wikis(getRealm).find(this)
   }

  /** get textual content, unprocessed, of this object, if found */
  def content = section.map{s=> page.flatMap(_.sections.find(_.name == s)).map(_.content) getOrElse s"[Section $s not found!]"} orElse page.map(_.content)

  /** withRealm - convienience builder. Note that you can't override a category prefix */
  def r(r:String) = if(Wikis.DFLT == r || CAT.unapply(cat).flatMap(_.realm).isDefined) this else this.copy(realm = Some(r))

  /** get the realm or the default - note taht the CAT prefix rules */
  def getRealm = realm orElse CAT.unapply(cat).flatMap(_.realm) getOrElse Wikis.DFLT

  /** find the ID for this page, if any - respects the NOCATS */
  def findId = {
    WikiIndex.withIndex(getRealm) { idx =>
      if(! cat.isEmpty)
        idx.get2(name, this)
      else {
        // try the nocats
        idx.get1k(name).filter(x=>WID.NOCATS.contains(x.cat)).headOption.flatMap(x=>idx.get2(name, x))
        //todo maybe forget this branch and enhance equals to look at nocats ?
      }
    } orElse Wikis.find(this).map(_._id)
  }

  def uwid = findId map {x=>UWID(cat, x, realm)}

  def cats = if(realm.exists(_ != Wikis.RK)) (realm.get + "." + cat) else cat

  /** format into nice url */
  def wpath: String = parentWid.map(_.wpath + "/").getOrElse("") + (
    if (cat != null && cat.length > 0 && !WID.NOCATS.contains(cat)) (cats + ":") else "") + name + (section.map("#" + _).getOrElse(""))
  /** this one used for simple cats with /w/:realm */
  def wpathnocats: String = parentWid.map(_.wpath + "/").getOrElse("") + (
    if (cat != null && cat.length > 0 && !WID.NOCATS.contains(cat)) (cat + ":") else "") + name + (section.map("#" + _).getOrElse(""))

  /** full categories allways */
  def wpathFull: String = parentWid.map(_.wpath + "/").getOrElse("") + (
    if (cat != null && cat.length > 0 ) (cats + ":") else "") + name + (section.map("#" + _).getOrElse(""))
  def formatted = this.copy(name=Wikis.formatName(this))
  def url: String = "http://" + Services.config.hostport + (realm.filter(_ != Wikis.RK).map(r=>s"/w/$r").getOrElse("")) + "/wiki/" + wpathnocats
  //todo this is stupid
  def urlRelative : String =
    (if(realm.isEmpty && Services.config.isLocalhost) s"/w/rk"
    else realm.filter(_ != Wikis.RK || Services.config.isLocalhost).map(r=>s"/w/$r").getOrElse("")) + "/wiki/" + wpathnocats
  def ahref: String = "<a href=\"" + url + "\">" + toString + "</a>"
  def ahrefRelative: String = "<a href=\"" + urlRelative + "\">" + toString + "</a>"

  /** helper to get a label, if defined or the default provided */
  //todo labels should not be in domain but in index...
  def label(id: String, alt: String) = WikiDomain(getRealm).labelFor(this, id).getOrElse(alt)
  def label(id: String) = WikiDomain(getRealm).labelFor(this, id).getOrElse(id)

}

object WID {
  /** do not require the category */
  private final val NOCATS = Array("Blog", "Post", "xSite")

  //cat:name#section$realm
  private val REGEX = """([^/:\]]*[:])?([^#|\]]+)(#[^|\]]+)?""".r

  /** @param a the list of wids from a path, parent to child */
  private def widFromSeg(a: Array[String]) = {
    val w = a.map { x =>
      x match {
        case REGEX(c, n, s) => WID(
          (if (c == null) "" else c.replaceFirst("[^.]+\\.", "")).replaceFirst(":", ""),
          n,
          None,
          Option(s).filter(_.length > 1).map(_.substring(1)),
          if(c != null && c.contains(".")) Some(c.replaceFirst("\\..*", "")) else None)
        case _ => UNKNOWN
      }
    }
    val res = w.foldLeft[Option[WID]](None)((x, y) => Some(WID(y.cat, y.name, x.flatMap(_.findId), y.section, y.realm)))
    res
  }

  /** parse WID from path */
  def fromPath(path: String): Option[WID] = {
    if (path == null || path.length() == 0)
      None
    else {
      val a = path.split("/")
      widFromSeg(a)
    }
  }

  /** parse CMDWID in path */
  def cmdfromPath(path: String): Option[CMDWID] = {
    if (path == null || path.length() == 0)
      None
    else {
      def splitIt (tag:String, path:String) = {
        val b = path split tag
        val a = b.head split "/"
        CMDWID(b.headOption, widFromSeg(a), tag.replaceAllLiterally("/", ""), b.tail.headOption.getOrElse(""))
      }

      // TODO optimize this copy/paste later
      //todo if the name contains the sequence /debug this won't work - should check i.e. /debug$
      Array("/xp/", "/xpl/", "/tag/").collectFirst {
        case tag if path contains tag => splitIt (tag, path)
      } orElse
        Array("/rss.xml", "/debug").collectFirst {
        case tag if path endsWith tag => splitIt (tag, path)
      } orElse
        Some(CMDWID(Some(path), widFromSeg(path split "/"), "", ""))
    }
  }

  val NONE = WID("?", "?")
  val UNKNOWN = WID("?", "?")
}
