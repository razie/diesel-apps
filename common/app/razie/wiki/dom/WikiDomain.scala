/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.dom

import razie.diesel.dom._
import razie.wiki.model._

import scala.collection.mutable.ListBuffer

/** encapsulates the knowledge to use the wiki-defined domain model
  *
  * extracts the domain from a reactor and activates it
  */
class WikiDomain (val realm:String, val wi:WikiInst) {

  final val WIKI_CAT = "wikiCategory"

  private var irdom : RDomain = null

  def rdom : RDomain = synchronized {
    if (irdom == null)
      irdom = Wikis(realm).pages("DslDomain").toList.flatMap(p=>WikiDomain.domFrom(p).toList).fold(toRdom)(_ plus _.revise)
    irdom
  }

  def resetDom = synchronized {
    irdom = null
  }

  import RDOM._

  def isWikiCategory(cat: String): Boolean = rdom.classes.values.exists(c=> c.name == cat && c.stereotypes.contains(WIKI_CAT))

  /** parse categories into domain model */
  def toRdom : RDomain = {
    val diamonds = for (cat <- wi.categories if cat.contentProps.exists(t=>t._1.startsWith("diamond:"))) yield {
      val x = cat.contentProps.find(t=>t._1.startsWith("diamond"))
    }

    // inherit and linearize classes in mixins
    val allClasses = new ListBuffer[WikiEntry]()
    allClasses appendAll wi.categories
    wi.fallBacks.foreach(
      _.categories.foreach(c=>
        if(!allClasses.exists(_.name == c.name)) allClasses.append(c)
      )
    )

    val classes = for (cat <- allClasses) yield {
      val assocs =
        for (
          t <- cat.contentProps if (t._1 startsWith "roles:");
          r <- t._2.split(",")
        ) yield {
          A("", cat.name, t._1.split(":")(1), "", r)
        }
      val base =
        for (
          t <- cat.contentProps if (t._1 startsWith "dom.base");
          r <- t._2.split(",")
        ) yield {
        r
      }
      C(cat.name, "", WIKI_CAT, base.toList, "", Nil, Nil, assocs.toList)
    }

    var x = new RDomain(realm, classes.map(c=>(c.name, c)).toMap, classes.flatMap(_.assocs).toList, List.empty, Map.empty)
//    x = wi.fallback.fold(x) {wi2=> x.plus(x.name, WikiDomain(wi2.realm).rdom)}
    x
  }

  /** aEnds that I link TO as role */
  def assocsWhereTheyHaveRole(cat: String, role: String) =
    rdom.assocs.filter(t=> t.z == cat && t.aRole == role).map(_.a) :::
      rdom.assocs.filter(t=> t.a == cat && t.zRole == role).map(_.z)

  /** aEnds that I link TO as role */
  def assocsWhereIHaveRole(cat: String, role: String) =
    rdom.assocs.filter(t=> t.z == cat && t.zRole == role).map(_.a) :::
    rdom.assocs.filter(t=> t.a == cat && t.aRole == role).map(_.z)

  /** aEnds that I link TO as role */
  def aEnds(zEnd: String, zRole: String) =
    rdom.assocs.filter(t=> t.z == zEnd && t.zRole == zRole).map(_.a)

  /** zEnds that link to ME and I have role */
  def zEnds(aEnd: String, zRole: String) =
    rdom.assocs.filter(t=> t.a == aEnd && t.zRole == zRole).map(_.z)

  def needsOwner(cat: String) =
    wi.category(cat).flatMap(_.contentProps.get("roles:" + "User")).exists(_.split(",").contains("Owner"))

  def noAds(cat: String) =
    wi.category(cat).flatMap(_.contentProps.get("noAds")).isDefined

  def needsParent(cat: String) =
    rdom.assocs.filter(t=> t.a == cat && t.zRole == "Parent" && !Array("User", "Person").contains(t.z)).map(_.z)

  def isA(what: String, cat:String) : Boolean =
    what == cat || rdom.classes.get(cat).toList.flatMap(_.base).foldLeft(false)((a,b) => a || isA(what, b))

  // todo optimize
  def dtree(base: String) : List[String] =
    (base :: rdom.classes.values.filter(_.base contains base).toList.flatMap(x=>dtree(x.name))).distinct

  def roles(a: String, z:String) : List[String] = {
    val mine = rdom.classes.get(a).toList.flatMap(_.assocs).filter(_.z == z).map(_.zRole)
    if(mine.isEmpty) rdom.classes.get(a).toList.flatMap(_.base).foldLeft(List.empty[String])((a,b) => a ++ roles(b, z))
    else mine
  }

}

object WikiDomain {//extends WikiDomain (Wikis.RK) {
  def apply (realm:String) = WikiReactors(realm).domain

  final val DOM_LIST = "dom.list"
  import RDOM._

  final val empty = new RDomain("EMPTY", Map.empty, Nil)

  /** crawl all domain pieces and build a domain */
  def domFrom (we:WikiEntry) : Option[RDomain] = {
    we.preprocessed
    val domList = we.cache.getOrElse(DOM_LIST, List[Any]()).asInstanceOf[List[Any]].reverse

    //    if(we.tags.contains(R_DOM) || we.tags.contains(DSL_DOM))
    Some(
      we.cache.getOrElseUpdate("dom", {
        var x=new RDomain("-",
          domList.collect {
            case c:C => (c.name, c)
          }.toMap,
          domList.collect {
            case c:A => c
          },
          domList.collect {
            case c:D if !c.isInstanceOf[A] => c
          },
          domList.collect {
            case o:O => (o.name, o)
          }.toMap,
          domList.collect {
            case f:F => (f.name, f)
          }.toMap)
        x.moreElements.appendAll(
          domList.filter {e=>
            !(e.isInstanceOf[A] ||
              e.isInstanceOf[C] ||
              e.isInstanceOf[D] ||
              e.isInstanceOf[O] ||
              e.isInstanceOf[F])
        })

        x
      }
      )) collect {
      case d:RDomain => d
    }
    //    else None
  }

  /** crawl all domain pieces and build a domain */
  def domFilter[T] (we:WikiEntry)(p:PartialFunction[Any,T]) : List[T] = {
    we.preprocessed
    //    if(we.tags.contains(R_DOM) || we.tags.contains(DSL_DOM))
    we.cache.getOrElse(DOM_LIST, List[Any]()).asInstanceOf[List[Any]].reverse.collect {
      case x if(p.isDefinedAt(x)) => p(x)
    }
  }

  WikiObservers mini {
    case e@WikiEvent(_, "WikiEntry", _, Some(x), _, _, _) if x.asInstanceOf[WikiEntry].category == "DslDomain" => {
      val we = x.asInstanceOf[WikiEntry]

      apply(we.realm).resetDom
    }
  }

  def canCreateNew (realm:String, cat:String) = "User" != cat && "WikiLink" != cat
  //todo can i create WIkiLink if I am admin?

  def spec (we:WikiEntry) = new DSpec {
    def wpath: String = we.wid.wpath

    def findTemplate(name: String): Option[DTemplate] =
      we.templateSections.find(_.name == name).map {t=>
        new DTemplate {
          def content : String = t.content
          def parms : String = t.signature
          def wpath : String = t.wid.wpath
          def pos : EPos = EPos(t.wid.copy(section = None).wpath, t.line, t.col)
        }
      }
  }
}


