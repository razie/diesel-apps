/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.dom

import razie.wiki.model._
import razie.wiki.util.js

/** DO NOT USE THIS PLEASE - it's in its very early stages
  *
  * encapsulates the knowledge to use the wiki-defined domain model */
class WikiDomain (realm:String, wi:WikiInst) {
  import razie.wiki.dom.DOM._

  //todo optimize
  private var idom : Domain = null

  /** have a neutral domain/meta model and load it from categories */
  def dom = {
    //todo async antip?
    if(idom == null) idom = wikid()
    idom
  }

  /** when dom classes edited */
  def resetDom = {
    //todo async antip?
    idom = null // wikid()
  }


  //todo rewrite these in terms of the neutral DOM

  /** get all zends as List (to, role) */
  def gzEnds(aEnd: String) =
    (for (
      c <- wi.categories if (c.contentTags.get("roles:" + aEnd).isDefined);
      t <- c.contentTags.get("roles:" + aEnd).toList;
      r <- t.split(",")
    ) yield (c.name, r)).toList

//    Wikis.categories.filter(_.contentTags.get("roles:" + aEnd).isDefined).flatMap(c=>c.contentTags.get("roles:" + aEnd).get.split(",").toList.map(x=>(c,x)))

  def gaEnds(zEnd: String) =
    for (
      c <- wi.category(zEnd).toList;
      t <- c.contentTags if (t._1 startsWith "roles:");
      r <- t._2.split(",")
    ) yield (t._1.split(":")(1), r)

  /** zEnds that link to ME as role */
  def zEnds(aEnd: String, role: String) =
    wi.categories.filter(_.contentTags.get("roles:" + aEnd).map(_.split(",")).exists(_.contains(role) || role=="")).toList

  /** aEnds that I link TO as role */
  def aEnds(zEnd: String, role: String) =
    for (
      c <- wi.category(zEnd).toList;
      t <- c.contentTags if (t._1.startsWith ("roles:") && (t._2.split(",").contains(role) || role==""))
    ) yield t._1.split(":")(1)

  def needsOwner(cat: String) =
    wi.category(cat).flatMap(_.contentTags.get("roles:" + "User")).exists(_.split(",").contains("Owner"))

  def noAds(cat: String) =
    wi.category(cat).flatMap(_.contentTags.get("noAds")).isDefined

  def needsParent(cat: String) =
    wi.category(cat).exists(_.contentTags.exists { t =>
      t._1.startsWith("roles:") && t._1 != "roles:User" && t._2.split(",").contains("Parent")
    })

  def labelFor(wid: WID, action: String) = wi.category(wid.cat) flatMap (_.contentTags.get("label." + action))

  /** parse categories into domain model */
  def wikid(): DOM.Domain = {
    val diamonds = for (cat <- wi.categories if cat.contentTags.exists(t=>t._1.startsWith("diamond:"))) yield {
      val x = cat.contentTags.find(t=>t._1.startsWith("diamond"))
    }
    val classes = for (cat <- wi.categories) yield {
      val assocs = for (t <- cat.contentTags if t._1 startsWith "roles:") yield {
        A(cat.name, t._1.split(":")(1), "", t._2)
      }
      C(cat.name, "?", Nil, Nil, assocs.toList)
    }

    Domain(realm, classes.map(c=>(c.name, c)).toMap, Map.empty)
  }
}

object WikiDomain {//extends WikiDomain (Wikis.RK) {
  def apply (realm:String) = Reactors(realm).domain
  val rk = Reactors("rk").domain
}

object DOM {
  // archtetypes
  val ARCH_SPEC = "Spec"
  val ARCH_ROLE = "Role"
  val ARCH_ENTITY = "Entity"
  val ARCH_MI = "MI"

  class CM // abstract Class Member
  case class P (name:String, ttype:String, multi:String, dflt:String, expr:String) extends CM // attr/parm spec
  case class F (name:String, parms:List[P]) extends CM // function/method
  case class V (name:String, value:String)  // attr value
  case class C (name:String, archetype:String, base:List[String], parms:List[P]=Nil, assocs:List[D]=Nil, methods:List[F]=Nil) //class
  case class O (name:String, base:String, parms:List[V]) { // object = instance of class
    def toJson = parms.map{p=> p.name -> p.value}.toMap
  }
       class D  (val roles:List[(String, String)], val ac:Option[AC]=None) //diamond association
  case class A  (a:String, z:String, aRole:String, zRole:String, override val ac:Option[AC]=None) //association
    extends D (List(a->aRole, z->zRole), ac)
  case class AC (name:String, a:String, z:String, aRole:String, zRole:String, parms:List[P]=Nil, methods:List[F]=Nil) //association class

   // Diamond Class

  case class E (name:String, parms:List[P], methods:List[F]) //event
  case class R (name:String, parms:List[P], body:String) //rule
  case class X (body:String) //expression

  case class Domain (name:String, classes:Map[String,C], objects:Map[String,O]) {

    // compose domains parsed in differnt places
    def plus (newName:String, other:Domain) = {
      Domain(newName, classes ++ other.classes, objects ++ other.objects)
    }

    /** simple json like representation of domain for browsing */
    def tojmap = {
      Map("name"->name,
        "classes" -> classes.values.toList.map{c=>
          Map(
            "name"->c.name,
            "parms" -> c.parms.map{p=>
              Map(
                "name"->p.name,
                "t" -> p.ttype
              )
            },
            "assocs" -> c.assocs.collect{
              case a : A => Map(
                "aname"     -> a.a,
                "zname"     -> a.z,
                "aRole"     -> a.aRole,
                "zRole"     -> a.zRole,
                "assocClass"-> a.ac.map(_.name).mkString
              )
              case d : D => Map(
                "roles"      -> d.roles.map{t=>Map("className"->t._1, "role"->t._2)},
                "assocClass" -> d.ac.map(_.name).mkString
              )
            }
          )},
        "objects" -> objects.values.toList.map{c=>
          Map(
            "name"  -> c.name,
            "parms" -> c.parms.toList.map{p=>
              Map(
                "name"  -> p.name,
                "value" -> p.value
              )
            }
          )}
      )
    }

//    def toJson = admin.js.tojson(tojmap)
    override def toString = js.tojsons(tojmap)
  }
}


