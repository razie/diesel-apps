package razie.diesel

import razie.wiki.dom.WikiDomain

import RDOM.{A, C, D, P, O, F}

import scala.collection.mutable.ListBuffer

class RDomain(val name: String, val classes: Map[String, C], val assocs: List[A], val diamonds:List[D] = List.empty, val objects: Map[String, O] = Map.empty, val funcs: Map[String, F] = Map.empty) {
  /** use this for other elements */
  val moreElements = new ListBuffer[Any]()

  def nz (s:String) = s != null && s.length > 0

  // compose domains parsed in differnt places
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
        A("", c.name, p.ttype, (if(p.ref.size == 0) "Parent" else ""), p.name)
      }
    }

    var x=new RDomain(name, classes, assocs ++ newAssocs, diamonds, objects, funcs)
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
          "parms" -> c.parms.toList.map { p =>
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
          "parms" -> c.parms.toList.map { p =>
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

object RDomain {
  final val DATA_TYPES = "String,Int,DateTime,JSON,XML,Image,URL,WID,UWID,WPATH".split(",")

  def isDataType (t:String) = t != null && (DATA_TYPES contains t)
}

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
}

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
        val bs = (
          (if(b.size > 0) b.take(b.size-1) else Nil).mkString("\n") + (if(b.size > 0) "\n  return "+b(b.size-1) else ""))
        val s = s"function ${f.name} (${f.parms.map(_.name).mkString(",")}) {$bs\n}"
        s
      }
    }
  }

  override def call (f:F) = {
    s"${f.name}(${f.parms.map(_.dflt).mkString(",")});"
  }
}

