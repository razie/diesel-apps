/**   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.dom

import razie.diesel.dom.RDOM.DE
import razie.hosting.WikiReactors
import razie.tconf.TagQuery
import razie.wiki.model._

/**
  * The domain for a realm. All sub-domains should be combined in this one domain.
  *
  * Combine all domain tools with higher level helpers on top of a domain
  */
trait WikiDomain {
  def realm: String

  def wi: WikiInst

  /** all plugins for this domain and in this realm */
  def allPlugins: List[DomInventory]

  def addPlugin(inv: DomInventory): List[DomInventory]

  def findPlugins(inventory: String, conn: String = ""): List[DomInventory] = {
    allPlugins.find(_.name == inventory).orElse(allPlugins.find(_.isInstanceOf[DefaultRDomainPlugin])).toList
  }

  /** based on annotations etc */
  def findPluginsForClass(c: DE): List[DomInventory] = {
    // todo get annotation "inventory"
    Nil
  }

  /** the aggregated domain representation for this realm */
  def rdom: RDomain

  /* while loading, it may recursively try to do some stuff - */
  def isLoading: Boolean

  def resetDom: Unit

  /** is this an actual wiki category or a user-defined class or imported concept? */
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
    rdom.classes.get(cat).flatMap(_.props.find(_.name == name).map(_.currentStringValue))

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
  final val WIKI_CAT = "wikiCategory"

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

  /** root categories we can create free instance from */
  def rootCats (realm:String) = {
    apply(realm)
        .rdom
        .classes
        .values
        .filter(_.stereotypes.contains(razie.diesel.dom.WikiDomain.WIKI_CAT))
        .map(_.name)
        .toList
  }

    /** present a WE as a generic spec */
  def spec (we:WikiEntry) = we

  /** if any special DOM wiki changes, rebuild the domain */
  WikiObservers mini {
    case WikiEvent(_, "WikiEntry", _, Some(x), _, _, _)
      if domTagQuery.matches(x.asInstanceOf[WikiEntry])
    => {
      val we = x.asInstanceOf[WikiEntry]

      WikiDomain.apply(we.realm).resetDom
    }
  }

  /** use with WikiSearch.getList */
  val domTagQuery = new TagQuery("DslDomain,dsldomain,Category,domain")
}


