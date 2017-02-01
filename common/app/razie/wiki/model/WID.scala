/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.model

import com.mongodb.casbah.Imports._
import com.novus.salat._
import org.bson.types.ObjectId
import razie.base.data.TripleIdx
import razie.clog
import razie.db.RazSalatContext._
import razie.wiki.{Services}
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
  lazy val wid = findWid orElse Some(WID(cat, id.toString).copy(realm=realm)) // used in too many places to refactor properly
  def nameOrId = wid.map(_.name).getOrElse(id.toString)
  lazy val grated = grater[UWID].asDBObject(this) //.copy(realm=None)) // todo I erase the realm for backwards compatibility
  lazy val page = Wikis(getRealm).find(this)

  /** get the realm or the default */
  def getRealm = realm.getOrElse(Wikis.DFLT)

  /** withRealm - convienience builder */
  def r(r:String) = if(Wikis.DFLT == r) this else this.copy(realm=Some(r))

  /** some topics don't use cats */
  override def equals (other:Any) = other match {
    case o: UWID => this.id == o.id // this way you can change cats without much impact
    case _ => false
  }
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
  *
  * NOTE: equals does not look at parent !!!
  */
case class WID(cat: String, name: String, parent: Option[ObjectId] = None, section: Option[String] = None, realm:Option[String]=None) {
  override def toString = "[[" + wpath + "]]"

  lazy val grated = grater[WID].asDBObject(this)
  lazy val findParent = parent flatMap (p => Wikis(getRealm).find(p))
  lazy val parentWid = parent flatMap (p => WikiIndex.withIndex(getRealm) { index => index.find { case (a, b, c) => c == p }.map(_._2) }) orElse (findParent map (_.wid))

  /** find a parent of the given category */
  def parentOf(category: String=>Boolean) = {
    def f(p: Option[WID]) = if (p.isEmpty) None else p.filter(x=>category(x.cat)).orElse(p.flatMap(_.parentWid))
    f(parentWid)
  }

  /** find the page for this, if any - respects the NOCATS */
  lazy val page = {
    val w = if (Services.config.cacheWikis) {
      import play.api.cache._
      import play.api.Play.current

      Cache.getAs[WikiEntry](this.wpath+".page").map { x =>
        clog << "WIKI_CACHED FULL-" + this.wpath
        x
      }
    } else None

    w.orElse {
      if (cat.isEmpty)
        findId flatMap Wikis(getRealm).find // special for NOCATS
      else
        Wikis(getRealm).find(this)
    }
  }

  def isEmpty = cat=="?" && name=="?" || cat=="-" && name=="-"

  def domain = Wikis.domain(getRealm)

  /** should this entry be indexed in memory */
  def shouldIndex = !(Wikis.PERSISTED contains cat)

  /** find the full section, if one is referenced */
  def findSection = section.flatMap{s=> page.flatMap(_.sections.find(_.name == s))}

  /** get textual content, unprocessed, of this object, if found */
  def content = section.map{s=>
    page.flatMap(p=> p.sections.find(_.name == s) orElse p.templateSections.find(_.name == s)).map(_.content) getOrElse s"`[Section $s not found in $toString!]`"
  } orElse page.map(_.content)

  /** withRealm - convienience builder. Note that you can't override a category prefix */
  def r(r:String) =
    if(CAT.unapply(cat).flatMap(_.realm).isDefined) this
    else this.copy(realm = Some(r))
//  def r(r:String) = if(Wikis.DFLT == r || CAT.unapply(cat).flatMap(_.realm).isDefined) this else this.copy(realm = Some(r))

  /** if wid has no realm, should get the realm or the default - note taht the CAT prefix rules */
  def getRealm = realm orElse CAT.unapply(cat).flatMap(_.realm) getOrElse Wikis.DFLT

  /** find the ID for this page, if any - respects the NOCATS */
  def findId  =
    if(ObjectId.isValid(name)) Some(new ObjectId(name))
    else findCatId().map(_._2)

  /** find the category, if missing */
  def findCat = findCatId().map(_._1)

  /** find the ID for this page, if any - respects the NOCATS */
  def findId  (curRealm:String) =
    if(ObjectId.isValid(name)) Some(new ObjectId(name))
    else findCatId(curRealm).map(_._2)

  /** find the category, if missing */
  def findCat (curRealm:String) = findCatId(curRealm).map(_._1)

  /** find the proper category and ID for this wid (name, or name and cat etc) */
  private def findCatId(curRealm:String="") = {
    def q = {idx: TripleIdx[String, WID, ObjectId] =>
      if(! cat.isEmpty)
        idx.get2(name, this).map((cat, _))
      else {
        // get by name and see if cat we found is NOCATS
        idx.get1k(name).filter(x=>
          WID.NOCATS.contains(x.cat)
        ).headOption.flatMap(x=>
          idx.get2(name, x)).map((cat, _)
        )
        //todo maybe forget this branch and enhance equals to look at nocats ?
      }
    }

    //todo performance of these is horrendous
    //todo i should ignore curRealm if I have my own
    // first current realm
    if(curRealm.isEmpty) {
      realm.orElse(CAT.unapply(cat).flatMap(_.realm)).map{r=>
        // was there a
        WikiIndex.withIndex(r)(q) orElse Wikis.find(this).map(x=>(x.category, x._id))
      } getOrElse {
        // try all indexes real quick
        var y : Option[(String, ObjectId)] = None
        WikiReactors.reactors.map(_._2.wiki.index).find{x=>
          x.withIndex(q).map{catid=>
            y = Some(catid)
            y
          }.isDefined
        }

        y orElse Wikis.find(this).map(x=>(x.category, x._id))
      }
    }
    else
      WikiIndex.withIndex(curRealm)(q) orElse WikiIndex.withIndex(getRealm)(q) orElse Wikis.find(this).map(x=>(x.category, x._id))
  }

  /** some topics don't use cats */
  override def equals (other:Any) = other match {
    case o: WID =>
      this.cat == o.cat && this.name == o.name &&
        (this.getRealm == o.getRealm ||
          this.realm.isEmpty ||
          o.realm.isEmpty)
    case _ => false
  }
  def uwid = findCatId() map {x=>UWID(x._1, x._2, realm)}

  /** cat with realm */
//  def cats = if(realm.exists(_ != Wikis.RK)) (realm.get + "." + cat) else cat
  def cats = if(realm.exists(_.length > 0)) (realm.get + "." + cat) else cat

  /** format into nice url */
  def wpath: String = parentWid.map(_.wpath + "/").getOrElse("") + (
    if (cat != null && cat.length > 0 && !WID.NOCATS.contains(cat)) (cats + ":") else "") + name + (section.map("#" + _).getOrElse(""))

  /** this one used for simple cats with /w/:realm */
  def wpathnocats: String = parentWid.map(_.wpath + "/").getOrElse("") + (
    if (cat != null && cat.length > 0 && !WID.NOCATS.contains(cat)) (cat + ":") else "") + name + (section.map("#" + _).getOrElse(""))

  /** full categories allways, with realm prefix if not RK */
  def wpathFull: String = parentWid.map(_.wpath + "/").getOrElse("") + (
    if (cat != null && cat.length > 0 ) (cats + ":") else "") + name + (section.map("#" + _).getOrElse(""))

  def formatted = this.copy(name=Wikis.formatName(this))
//  def url: String = "http://" + Services.config.hostport + (realm.filter(_ != Wikis.RK).map(r=>s"/w/$r").getOrElse("")) + "/wiki/" + wpathnocats

  /** the canonical URL with the proper hostname for reactor */
  def url: String = {
    val hasRealm = realm.isDefined && WikiReactors(realm.get).websiteProps.prop("domain").exists(_.length > 0)

    "http://" + {
      if (hasRealm && !Services.config.isLocalhost)
        WikiReactors(realm.get).websiteProps.prop("domain").get
      else
        //todo current realm
        Services.config.hostport /* + realm.map(r => s"/w/$r").getOrElse("")*/
    } + "/" + {
      if(realm.isDefined) "wiki/" + wpath
      else canonpath //wpathnocats
    }
  }

  //todo this is stupid
  def urlRelative : String = urlRelative(Wikis.RK)

  /** use when coming from a known realm */
  def urlRelative (fromRealm:String) : String =
  "/" + {
    if(realm.exists(_ != fromRealm)) "wiki/" + wpath
    else canonpath //wpathnocats
  }

  /** canonical path - may be different from wpath */
  def canonpath =
    if(parent.isEmpty && WID.PATHCATS.contains(cat))
      s"$cat/$name" + (section.map("#" + _).getOrElse(""))
    else
      s"wiki/$wpathnocats"

  def ahref: String = "<a href=\"" + url + "\">" + toString + "</a>"
  def ahrefRelative (fromRealm:String=Wikis.RK): String = "<a href=\"" + urlRelative(fromRealm) + "\">" + toString + "</a>"
  def ahrefNice (fromRealm:String=Wikis.RK): String = "<a href=\"" + urlRelative(fromRealm) + "\">" + getLabel() + "</a>"

  /** helper to get a label, if defined or the default provided */
  //todo labels should not be in domain but in index...
  def label(action: String, alt: String) = WikiReactors(getRealm).wiki.labelFor(this, action).getOrElse(alt)
  def label(action: String) = WikiReactors(getRealm).wiki.labelFor(this, action).getOrElse(action)

  // this is my label
  def getLabel() = WikiReactors(getRealm).wiki.label(this)
}

/** wid utils */
object WID {
  /** SEO optimization - WIDs in these categories do not require the category in wpath */
  final val NOCATS = Array("Blog", "Post")
  final val PATHCATS = Array("Club", "Pro", "School", "Talk", "Topic", "Session", "Drill", "Pathway")// timid start to shift to /cat/name

  //cat:name#section
  private val REGEX = """([^/:\]]*[:])?([^#|\]]+)(#[^|\]]+)?""".r

  /** parse a wid
    * @param a the list of wids from a path, parent to child */
  private def widFromSeg(a: Array[String], curRealm:String = "") = {
    val w = a.map { x =>
      x match {
        case REGEX(c, n, s) => {
          val cat =
            if (c == null) ""
            else c.replaceFirst("[^.]+\\.", "").replaceFirst(":", "")
          val name =
            if(n==null) ""
            else if(cat.length <= 0) n.replaceFirst("[^.]+\\.", "")
            else n
          WID(
            cat,
            name,
            None,
            Option(s).filter(_.length > 1).map(_.substring(1)),
            if(c != null && c.contains("."))
              Some(c.replaceFirst("\\..*", ""))
            else if(cat.length <= 0 && n != null && n.contains("."))
              // only if cat is not specified
              Some(n.replaceFirst("\\..*", ""))
            else None)
        }
        case _ => empty
      }
    }
    val res = w.foldLeft[Option[WID]](None)((x, y) => Some(WID(y.cat, y.name, x.flatMap(_.findId(curRealm)), y.section,
      //always follow parent's realm if provided
      x.flatMap(_.realm).orElse(y.realm))))
    res
  }

  /** parse WID from path */
  def fromPath(path: String) : Option[WID] = fromPath(path, "")

  /** parse WID from path */
  def fromPath(path: String, curRealm:String): Option[WID] = {
    if (path == null || path.length() == 0)
      None
    else {
      val a = path.split("/")
      widFromSeg(a, curRealm)
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
      Array("/xp/", "/xpl/", "/tag/", "/react/").collectFirst {
        case tag if path contains tag => splitIt (tag, path)
      } orElse
        Array("/rss.xml", "/debug").collectFirst {
        case tag if path endsWith tag => splitIt (tag, path)
      } orElse
        Some(CMDWID(Some(path), widFromSeg(path split "/"), "", ""))
    }
  }

  final val wikip2 = """\[\[alias:([^\]]*)\]\]"""
  final val wikip2r = wikip2.r

  /** is this just an alias?
    *
    * an alias is a topic that only contains the alias markup: [[alias:xxx]]
    */
  def alias (content:String) : Option[WID] = {
    if (wikip2r.findFirstMatchIn(content).isDefined) {
//    if (content.matches(wikip2)) {
      val wikip2r(wpath) = content
      WID.fromPath(wpath)
    } else None
  }

  /** is this just an alias?
    *
    * an alias is a topic that only contains the alias markup: [[alias:xxx]]
    *
    * todo is it faster to check startsWith and then pattern?
    */
  def isAlias (content:String) : Boolean =
    content.startsWith("[[alias:") && wikip2r.findFirstMatchIn(content).isDefined

  final val empty = WID("-", "-")
}
