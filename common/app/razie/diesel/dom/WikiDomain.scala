package razie.diesel.dom

import razie.diesel.ext
import razie.tconf.{DTemplate, SpecPath}
import razie.wiki.model._

/**
  * encapsulates the knowledge to use the wiki-defined domain model
  */
trait WikiDomain {
  def realm:String
  def wi:WikiInst

  final val WIKI_CAT = "wikiCategory"

  def rdom : RDomain

  def resetDom : Unit

  def isWikiCategory(cat: String): Boolean

  /** parse categories into domain model */
  def createRDom : RDomain

  /** aEnds that I link TO as role */
  def assocsWhereTheyHaveRole(cat: String, role: String) : List[String] =
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
    rdom.assocs.exists(t=> t.a == cat && t.z == "User" && t.zRole == "Owner")

  def prop(cat: String, name:String) : Option[String] =
    rdom.classes.get(cat).flatMap(_.props.find(_.name == name).map(_.dflt))

  def noAds(cat: String) =
    prop(cat, "noAds").isDefined

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

object WikiDomain {

  def apply(realm: String) = WikiReactors(realm).domain

  /** todo does it really need to start with one */
  def domFrom (first:WikiEntry, pages:List[WikiEntry]) : RDomain = {
    RDomain.domFrom(first, pages)
  }

  /** crawl all domain pieces and build a domain */
  def domFrom (we:WikiEntry) : Option[RDomain] = {
    we.preprocessed
    RDomain.domFrom(we)
  }

  /** crawl all domain pieces and build a domain */
  def domFilter[T] (we:WikiEntry)(p:PartialFunction[Any,T]) : List[T] = {
    RDomain.domFilter(we)(p)
  }

  def canCreateNew (realm:String, cat:String) = "User" != cat && "WikiLink" != cat
  //todo can i create WIkiLink if I am admin?

  /** present a WE as a generic spec */
  def spec (we:WikiEntry) = we

  WikiObservers mini {
    case WikiEvent(_, "WikiEntry", _, Some(x), _, _, _)
      if x.asInstanceOf[WikiEntry].category == "DslDomain" &&
        x.asInstanceOf[WikiEntry].category == "Category" &&
        x.asInstanceOf[WikiEntry].tags.contains("dsldomain")
    => {
      val we = x.asInstanceOf[WikiEntry]

      WikiDomain.apply(we.realm).resetDom
    }
  }
}

case class WikiDTemplate (t:WikiSection) extends DTemplate {
  def content : String = t.content
  def parmStr : String = t.signature
  def parms : Map[String,String] = t.args
  def specPath = SpecPath("local", t.wid.wpath, t.wid.getRealm)
  def pos : EPos = EPos(t.wid.copy(section = None).wpath, t.line, t.col)

}


