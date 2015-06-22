package razie.diesel

import razie.wiki.dom.WikiDomain

import RDOM.{A, C, D, P, O}

class RDomain(val name: String, val classes: Map[String, C], val assocs: List[A], val diamonds:List[D] = List.empty, val objects: Map[String, O] = Map.empty) {

  def nz (s:String) = s != null && s.length > 0

  // compose domains parsed in differnt places
  def plus(other: RDomain, newName:String=name) = {
    new RDomain(newName, classes ++ other.classes, assocs ++ other.assocs, diamonds ++ other.diamonds, objects ++ other.objects)
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

    new RDomain(name, classes, assocs ++ newAssocs, diamonds, objects)
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
      }
    )
  }

  def needsOwner(name: String) = false

  def needsParent(name: String) = false

  override def toString = razie.js.tojsons(tojmap)
}

object RDomain {
  final val DATA_TYPES = "String,Int,DateTime,JSON,XML,Image,URL,WID,UWID,WPATH".split(",")

  def isDataType (t:String) = t != null && (DATA_TYPES contains t)
}

