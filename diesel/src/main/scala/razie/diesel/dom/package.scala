package razie.diesel

import razie.diesel.dom.RDOM.P
import razie.js
import razie.wiki.Enc

/**
 * simple, neutral domain model representation: class/object/function
 *
 * These are collected in RDomain
 */
package object dom {
  // archtetypes
  val ARCH_SPEC = "Spec"
  val ARCH_ROLE = "Role"
  val ARCH_ENTITY = "Entity"
  val ARCH_MI = "MI"

  /** like an Option from plain strings */
  def smap(s:String) (f:String=>String) =  if(s != null && s.length > 0) f(s) else ""
  def mks(l:List[_], pre:String, sep:String, post:String="", indent:String="") = if(l.size>0) pre + l.map(indent + _).mkString(sep) + post else ""
  def span(s: String, k: String = "default") = s"""<span class="label label-$k">$s</span>"""

  def quot(s:String) = "\""+ s + "\""
  def escapeHtml(s:String) = Enc.escapeHtml(s)

  /** if you have a func defn handy */
  def qTyped(q:Map[String,String], f:Option[RDOM.F]) = q.map { t =>
    def prep(v: String) =
      if (v.startsWith("\"")) v.replaceAll("\"", "")
      else if (v.startsWith("\'")) v.replaceAll("\'", "")
      else v

    val p = f.flatMap(_.parms.find(_.name == t._1))
    if (p.exists(x=> x.ttype == "Int" || x.ttype == WTypes.NUMBER)) (t._1+"",
      try {
        t._2.toDouble
      } catch {
        case e:Throwable => throw new IllegalArgumentException("Type error: expected Int, parm "+t._1+" found "+t._2)
      }
    )
    else
      (t._1, prep(t._2))
  }

  /** better version - f is if you have a func defn in context */
  def qTypedP(q:Map[String,P], f:Option[RDOM.F]) = q.map { t =>
    def prep(v: String) =
      if (v.startsWith("\"")) v.replaceAll("\"", "")
      else if (v.startsWith("\'")) v.replaceAll("\'", "")
      else v

    val p = f.flatMap(_.parms.find(_.name == t._1)).getOrElse(t._2)

    if (p.ttype == "Int" || p.ttype == WTypes.NUMBER) (t._1+"",
      try {
        t._2.dflt.toDouble
      } catch {
        case _:Throwable => throw new IllegalArgumentException("Type error: expected Int, parm "+t._1+" found "+t._2)
      }
      )
//      this is not working - maybe at some point. for now do js:JSON.parse()
//    else if (p.ttype == WTypes.JSON || p.ttype == WTypes.appJson) (t._1+"",
//      try {
//        // nashorn allows maps to be accessed like properties, see https://stackoverflow.com/questions/31292201/effective-way-to-pass-json-between-java-and-javascript
//        js.parse(t._2.dflt)
//      } catch {
//        case _:Throwable => throw new IllegalArgumentException("Type error: expected JSON, parm "+t._1+" found "+t._2)
//      }
//    )
    else
      (t._1, prep(t._2.dflt))
  }

}
