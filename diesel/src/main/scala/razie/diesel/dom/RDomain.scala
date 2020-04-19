/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.dom

import razie.diesel.dom.RDOM._
import razie.diesel.engine.nodes.EVal
import razie.tconf.DSpec
import scala.collection.mutable.ListBuffer

/** a domain or sub-domain specification. Think UML.
  *
  * in DDD parlance, this is a bounded domain.
  *
  * these are composable: bigger = root plus more
  */
class RDomain(

  val name: String,
  val classes: Map[String, C],
  val assocs: List[A],
  val diamonds:List[D] = List.empty,
  val objects: Map[String, O] = Map.empty,
  val funcs: Map[String, F] = Map.empty) {

  /** use this for other elements */
  val moreElements = new ListBuffer[Any]()

  def nz (s:String) = s != null && s.length > 0

  /** compose domains parsed in different places */
  def plus(other: RDomain, newName:String=name) = {
    var x=new RDomain(newName, classes ++ other.classes, assocs ++ other.assocs, diamonds ++ other.diamonds, objects ++ other.objects, funcs ++ other.funcs)
    x.moreElements.appendAll(moreElements)
    x.moreElements.appendAll(other.moreElements)
    x
  }

  /** clean the model - generate new assocs from members etc */
  def revise = {
    val newAssocs = classes.values.toList.flatMap{c=>
      c.parms.filter(p=> nz(p.ttype) &&
        !RDomain.isDataType(p.ttype) &&
        !assocs.exists(a=>a.a == c.name && a.z==p.ttype && a.zRole==p.name)).map{p=>
        A("", c.name, p.ttype, (if(!p.ttype.isRef) "Parent" else ""), p.name)
      }
    }

    val x=new RDomain(name, classes, assocs ++ newAssocs, diamonds, objects, funcs)
    x.moreElements.appendAll(moreElements)
    x
  }

  /** add a fake Domain root so you can start browsing it there */
  def addRoot = this plus new RDomain("root", Map("Domain" -> C("Domain", "", "", Nil, "")),
    classes.values.toList.map(c=>A("", "Domain", c.name, "root", "fake")))

  /** simple json like representation of domain for browsing */
  def tojmap = {
    Map("name" -> name,
      "classes" -> classes.values.toList.map { c =>
        Map(
          "name" -> c.name,
          "parms" -> c.parms.map { p =>
            Map(
              "name" -> p.name,
              "t" -> p.ttype
            )
          },
          "assocs" -> c.assocs.map { a => Map(
              "aname" -> a.a,
              "zname" -> a.z,
              "aRole" -> a.aRole,
              "zRole" -> a.zRole,
              "assocClass" -> a.ac.map(_.name).mkString
            )
          },
        "props" -> c.props.map { p =>
          Map(
            "name" -> p.name,
            "dflt" -> p.dflt
          )
        }
        )
      },
      "assocs" -> assocs.map {a=> Map(
          "aname" -> a.a,
          "zname" -> a.z,
          "aRole" -> a.aRole,
          "zRole" -> a.zRole,
          "assocClass" -> a.ac.map(_.name).mkString
        )
      },
      "diamonds" -> diamonds.map {d=> Map(
          "roles" -> d.roles.map { t => Map("className" -> t._1, "role" -> t._2) },
          "assocClass" -> d.ac.map(_.name).mkString
        )
      },
      "objects" -> objects.values.toList.map { c =>
        Map(
          "name" -> c.name,
          "parms" -> c.parms.map { p =>
            Map(
              "name" -> p.name,
              "value" -> p.value
            )
          }
        )
      },
      "funcs" -> funcs.values.toList.map { c =>
        Map(
          "name" -> c.name,
          "parms" -> c.parms.map { p =>
            Map(
              "name" -> p.name,
              "t" -> p.ttype
            )
          }
        )
      }
    )
  }

  def needsOwner(name: String) = false

  def needsParent(name: String) = false

  override def toString = razie.js.tojsons(tojmap)

  def mkCompiler(lang:String) : RCompiler = {
    if(lang == "js") new RJSCompiler(this)
    else throw new IllegalArgumentException("can't comile to language "+lang)
  }

}

/** some helpers */
object RDomain {
  final val DATA_TYPES = "String,Int,DateTime,JSON,XML,Image,URL,WID,UWID,WPATH".split(",")

  /** we cache key */
  final val DOM_LIST = "dom.list"
  final val DOM_ANNO_LIST = "dom.anno.list"

  /** an empty domain - use it to fold */
  final val empty = new RDomain("EMPTY", Map.empty, Nil)

  def isDataType (t:String) = t != null && (DATA_TYPES contains t)

  /** todo does it really need to start with one */
  def domFrom (first:DSpec, pages:List[DSpec]) : RDomain = {
    val dom = pages.flatMap(p=>
      domFrom(p).toList
    ).foldLeft(domFrom(first).get)((a, b) => a.plus(b)).revise.addRoot
    dom
  }

  /** crawl all domain pieces and build a domain */
  def domFrom (we:DSpec) : Option[RDomain] = {
    // it will always make a point of calling parsed before looking in the cache
    if(we.parsed.contains("CANNOT PARSE"))
      we.collector.put(
        DOM_LIST,
        List(
          EVal(
            P("error", "ERROR: "+we.parsed))))

    val domList = we.collector.getOrElse(DOM_LIST, List[Any]()).asInstanceOf[List[Any]].reverse

    // this causes the underlying fire to avoid fallen capter Y and focus on fighter 2

    //    if(we.tags.contains(R_DOM) || we.tags.contains(DSL_DOM))
    Some(
      we.collector.getOrElseUpdate("razie/diesel/dom/dom", {
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
        // now collect everything else in more
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
  def domFilter[T] (we:DSpec)(p:PartialFunction[Any,T]) : List[T] = {
    //    if(!we.cache.contains(DOM_LIST) && we.preprocessed.s.contains("CANNOT PARSE"))
    // todo this can keep adding errors - check that there isn't one already
    if(we.parsed.contains("CANNOT PARSE"))
      we.collector.put(
        DOM_LIST,
        List(
          EVal(
            P("error", "ERROR: "+we.parsed))))

    we.collector.getOrElse(DOM_LIST, List[Any]()).asInstanceOf[List[Any]].reverse.collect {
      case x if(p.isDefinedAt(x)) => p(x)
    }
  }
}

/** base idea of a compiler - implement one per language and - I don't remember how they're plugged in
  *
  * todo compiler factory
  */
trait RCompiler {
  def lang : String

  // c.compileAll ( c.not {case fx:RDOM.F if fx.name == f.name => true})
  def not (filter: PartialFunction[Any,Boolean]) : PartialFunction[Any,Boolean] = {
    case x if filter.isDefinedAt(x) => !filter(x)
    case x if ! filter.isDefinedAt(x) => true
  }

  def compileAll (filter: PartialFunction[Any,Boolean]) : String
  def compile (elem:Any) : String
  def call (f:F) : String
  def callInContext (f:F) : String
}

/** JS compiler - used for domain functionality defined in JS
  */
class RJSCompiler (val dom:RDomain) extends RCompiler {
  override def lang = "js"

  override def compileAll (filter: PartialFunction[Any,Boolean]) = {
    val f = dom.funcs.values.filter(filter).map(compile).mkString("\n")
    f
  }

  override def compile (elem:Any) = {
    elem match {
      case f:F => {
        // prepare the func body - put a return on it and stuff
        val b = f.script.lines.toList
        val bs = b.mkString("\n")
//        val bs = (
//          (if(b.size > 0) b.take(b.size-1) else Nil).mkString("\n") +
//            (if(b.size > 0) "\n  return "+b(b.size-1) else ""))
        val s = s"function ${f.name.replaceAllLiterally(".", "_")} (${f.parms.map(_.name).mkString(",")}) {$bs\n}"
        s
      }
    }
  }

  override def call (f:F) = {
    s"${f.name.replaceAllLiterally(".", "_")}(${f.parms.map(_.currentStringValue).mkString(",")});"
  }


  // call assuming all the parms are vars in context
  override def callInContext (f:F) = {
    s"${f.name.replaceAllLiterally(".", "_")}(${f.parms.map(_.name).mkString(",")});"
  }
}

