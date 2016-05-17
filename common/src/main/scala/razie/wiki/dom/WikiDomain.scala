/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.dom

import razie.diesel.{RDOM, RDomain}
import razie.wiki.admin.{WikiEvent, WikiObservers}
import razie.wiki.model._

/** encapsulates the knowledge to use the wiki-defined domain model
  *
  * extracts the domain from a reactor and activates it
  */
class WikiDomain (val realm:String, val wi:WikiInst) {

  final val WIKI_CAT = "wikiCategory"

  private var irdom : RDomain = null

  def rdom : RDomain = synchronized {
    if (irdom == null)
      irdom = Wikis(realm).pages("DslDomain").toList.flatMap(p=>WikiDomain.domFrom(p).toList).fold(toRdom(realm))(_ plus _.revise)
    irdom
  }

  def resetDom = synchronized {
    irdom = null
  }

  import RDOM._

  def isWikiCategory(cat: String): Boolean = rdom.classes.values.exists(c=> c.name == cat && c.stereotypes.contains(WIKI_CAT))

  /** parse categories into domain model */
  def toRdom(realm:String): RDomain = {
    val diamonds = for (cat <- wi.categories if cat.contentProps.exists(t=>t._1.startsWith("diamond:"))) yield {
      val x = cat.contentProps.find(t=>t._1.startsWith("diamond"))
    }

    val classes = for (cat <- wi.categories) yield {
      val assocs =
        for (
          t <- cat.contentProps if (t._1 startsWith "roles:");
          r <- t._2.split(",")
        ) yield {
        A("", cat.name, t._1.split(":")(1), "", r)
      }
      C(cat.name, "", WIKI_CAT, Nil, "", Nil, Nil, assocs.toList)
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
    rdom.assocs.filter(t=> t.a == cat && t.zRole == "Parent").map(_.z)

}

object WikiDomain {//extends WikiDomain (Wikis.RK) {
  def apply (realm:String) = Reactors(realm).domain

  final val DOM_LIST = "dom.list"
  import RDOM._

  final val empty = new RDomain("EMPTY", Map.empty, Nil)

  /** crawl all domain pieces and build a domain */
  def domFrom (we:WikiEntry) : Option[RDomain] = {
    we.preprocessed
    //    if(we.tags.contains(R_DOM) || we.tags.contains(DSL_DOM))
    Some(
      we.cache.getOrElseUpdate("dom",
        new RDomain("?",
          we.cache.getOrElse(DOM_LIST, List[Any]()).asInstanceOf[List[Any]].collect {
            case c:C => (c.name, c)
          }.toMap,
          we.cache.getOrElse(DOM_LIST, List[Any]()).asInstanceOf[List[Any]].collect {
            case c:A => c
          },
          we.cache.getOrElse(DOM_LIST, List[Any]()).asInstanceOf[List[Any]].collect {
            case c:D if !c.isInstanceOf[A] => c
          },
          we.cache.getOrElse(DOM_LIST, List[Any]()).asInstanceOf[List[Any]].collect {
            case o:O => (o.name, o)
          }.toMap)
      )) collect {
      case d:RDomain => d
    }
    //    else None
  }

  WikiObservers mini {
    case e@WikiEvent(_, "WikiEntry", _, Some(x), _, _) if x.asInstanceOf[WikiEntry].category == "DslDomain" => {
      val we = x.asInstanceOf[WikiEntry]

      apply(we.realm).resetDom
    }
  }

  def canCreateNew (realm:String, cat:String) = "User" != cat && "WikiLink" != cat
  //todo can i create WIkiLink if I am admin?
}


