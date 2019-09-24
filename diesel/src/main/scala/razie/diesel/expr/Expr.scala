package razie.diesel.expr

import mod.diesel.model.exec.EESnakk
import org.json.JSONObject
import razie.clog
import razie.diesel.dom.RDOM.{P, PValue}
import razie.diesel.dom._
import razie.diesel.engine.DomEngine
import razie.diesel.exec.EEFunc
import razie.diesel.ext.{CanHtml, EMsg}
import razie.wiki.parser.SimpleExprParser
import scala.collection.mutable
import scala.util.Try
import scala.util.parsing.json.JSONArray

//------------ expressions and conditions

/** marker exception class for expr */
class DieselExprException (msg:String) extends RuntimeException (msg)

/** deserialization is assumed via DSL
  *
  *  the idea is that all activities would have an external DSL form as well
  *  and can serialize themselves in that form
  *
  *  serialize the DEFINITION only - not including states/values
  */
trait HasDsl /*extends GReferenceable*/ {
  def serialize: String = toDsl

  /** serialize the DEFINITION - not including
    */
  def toDsl: String
  def toIndentedDsl(indent: Int => String, level: Int) = indent(level) + toDsl
}

/**
  * Basic executable/actionable interface. These process a default input value and return a default output value.
  *
  * They are also invoked in a context - a set of objects in a certain role.
  *
  * There are two major branches: WFunc and WfActivity. An action is a workflow specific thing and is aware of next actions, state of execution whatnot. It also does something so it's derived from WFunc.
  *
  * WFunc by itself only does something and is not stateful. Most activities are like that.
  */
trait WFunc { // extends PartialFunc ?
  /** apply this function to an input value and a context */
  def apply(v: Any)(implicit ctx: ECtx): Any

  /** apply this function to an input value and a context */
  def applyTyped(v: Any)(implicit ctx: ECtx): P = {
    val res = apply(v)
    // todo use the PValue
//    P("", res.toString, getType)
    P.fromTypedValue("", res, getType)
  }

  /** what is the resulting type - when known */
  def getType: String = ""
}

/** an expression */
abstract class Expr extends WFunc with HasDsl with CanHtml {
  def expr: String
  override def toString = toDsl
  override def toDsl = expr
  override def toHtml = tokenValue(toDsl)
}


/** a function */
case class ExprRange(val start: Expr, val end: Option[Expr]) extends Expr {
  /** what is the resulting type - when known */
  override def getType: String = WTypes.RANGE
  override def expr = toString

  override def apply(v: Any)(implicit ctx: ECtx) = applyTyped(v).calculatedValue

  override def applyTyped(v: Any)(implicit ctx: ECtx): P = {
    val as = start.apply(v)
    // todo should apply typed and verify the type is INT

    end.map { end =>
      val zs = end.apply(v)
      P.fromTypedValue("", Range(as.toString.toInt, zs.toString.toInt), WTypes.RANGE)
    }.getOrElse {
      P.fromTypedValue("", Range(as.toString.toInt, scala.Int.MaxValue), WTypes.RANGE)
    }
  }

  override def toDsl = start + ".." + end
  override def toHtml = tokenValue(toDsl)
}

/** a function */
case class CExprNull() extends Expr {
  override def getType: String = WTypes.UNDEFINED
  override def expr = "null"

  override def apply(v: Any)(implicit ctx: ECtx) = applyTyped(v).calculatedValue

  override def applyTyped(v: Any)(implicit ctx: ECtx): P = P("", "", WTypes.UNDEFINED)

  override def toDsl = expr
  override def toHtml = expr
}


/**
  * constant expression - similar to PValue
  */
case class CExpr[T](ee: T, ttype: String = "") extends Expr {
  val expr = ee.toString

  override def apply(v: Any)(implicit ctx: ECtx) = applyTyped(v).currentStringValue

  override def applyTyped(v: Any)(implicit ctx: ECtx): P = {
    val es = P.asString(ee)

    if (ttype == WTypes.NUMBER) {
      if (es.contains("."))
        P("", es, ttype).withValue(es.toDouble, WTypes.NUMBER)
      else
        P("", es, ttype).withValue(es.toInt, WTypes.NUMBER)
    } else if (ttype == WTypes.BOOLEAN) {
      P("", es, ttype).withValue(es.toBoolean, WTypes.BOOLEAN)
    } else {
      // expand templates by default
      if (es contains "${") {
        var s1 = ""
        try {
          val PAT = """\$\{([^\}]*)\}""".r
          val eeEscaped = es
            .replaceAllLiterally("(", """\(""")
            .replaceAllLiterally(")", """\)""")
          s1 = PAT.replaceAllIn(es, {
            m =>
              (new SimpleExprParser).parseExpr(m.group(1)).map { e =>
                val res = P("x", "", "", "", "", Some(e)).calculatedValue

                // if the parm's value contains $ it would not be expanded - that's enough, eh?
                // todo recursive expansions?
                val escaped = res
                  .replaceAllLiterally("\\", "\\\\")
                  .replaceAll("\\$", "\\\\\\$")

                escaped
              } getOrElse
                s"{ERROR: ${m.group(1)}"
          })
        } catch {
          case e: Exception =>
            throw new DieselExprException(s"REGEX err for $es ")
              .initCause(e)
        }
        P("", s1, ttype)
      } else
        P("", es, ttype).withCachedValue(ee, ttype, es)
    }
  }

  override def toDsl = if (ttype == "String") ("\"" + expr + "\"") else expr
  override def getType: String = ttype
  override def toHtml = tokenValue(escapeHtml(toDsl))
}

/** arithmetic expressions */
case class AExpr(val expr: String) extends Expr {
  override def apply(v: Any)(implicit ctx: ECtx) = v
}

/** a block */
case class BlockExpr(ex: Expr) extends Expr {
  val expr = "( " + ex.toString + " )"
  override def apply(v: Any)(implicit ctx: ECtx) = ex.apply(v)
  override def getType: String = ex.getType
}

/** a js expression
  * js:a.b
  * js:{...}
  */
case class JSSExpr(s: String) extends Expr {
  val expr = "js{{ " + s + " }}"

  override def getType: String = WTypes.UNKNOWN

  override def apply(v: Any)(implicit ctx: ECtx) =
    EEFunc.execute(s) //.dflt

  override def applyTyped(v: Any)(implicit ctx: ECtx): P = {
      EEFunc.executeTyped(s)
  }
}

/** a scala expression
  * sc:a.b
  * sc:{...}
  */
case class SCExpr(s: String) extends Expr {
  val expr = "sc{{ " + s + " }}"

  override def getType: String = WTypes.UNKNOWN

  override def apply(v: Any)(implicit ctx: ECtx) = ???

  override def applyTyped(v: Any)(implicit ctx: ECtx): P = ???
}

/** a json block */
case class JBlockExpr(ex: List[(String, Expr)]) extends Expr {
  val expr = "{" + ex.map(t=>t._1 + ":" + t._2.toString).mkString(",") + "}"

  override def apply(v: Any)(implicit ctx: ECtx) = applyTyped(v).currentStringValue

  override def applyTyped(v: Any)(implicit ctx: ECtx) = {
    // todo this can be way faster for a few types, like Array - $send ctx.set (state281 = {source: [0,1,2,3,4,5], dest: [], aux: []})
//    val orig = template(expr)
    val orig = ex
      .map(t=> (t._1, t._2.applyTyped(v)))
      .map(t=> (t._1, t._2 match {
        case p@P(n,d,WTypes.NUMBER, _, _, _, Some(PValue(i:Int, _, _))) => i
        case p@P(n,d,WTypes.NUMBER, _, _, _, Some(PValue(i:Double, _, _))) => i

        case p@P(n,d,WTypes.BOOLEAN, _, _, _, Some(PValue(b:Boolean, _, _))) => b

        case p:P => p.currentStringValue match {
          case i: String if i.trim.startsWith("[") && i.trim.endsWith("]") => i
          case i: String if i.trim.startsWith("{") && i.trim.endsWith("}") => i
          case i: String => "\"" + i + "\""
        }

      }))
      .map(t=> s""" "${t._1}" : ${t._2} """)
      .mkString(",")
    // parse and clean it up so it blows up right here if invalid
    val j = new JSONObject(s"{$orig}")
    P.fromTypedValue("", j, WTypes.JSON)
//    new JSONObject(s"{$orig}").toString(2)
  }

  override def getType: String = WTypes.JSON

  // replace ${e} with value
  def template(s: String)(implicit ctx: ECtx) = {

    EESnakk.prepStr2(s, Nil)
  }
}

/** a json block */
case class JArrExpr(ex: List[Expr]) extends Expr {
  val expr = "[" + ex.mkString(",") + "]"

  override def apply(v: Any)(implicit ctx: ECtx) = {
//    val orig = template(expr)
    val orig = ex.map(_.apply(v)).mkString(",")
    // parse and clean it up so it blows up right here if invalid
    new org.json.JSONArray(s"[$orig]").toString()
  }

  override def applyTyped(v: Any)(implicit ctx: ECtx): P = {
    val orig = ex.map(_.apply(v)).mkString(",")
    // parse and clean it up so it blows up right here if invalid
    val ja = new org.json.JSONArray(s"[$orig]")
    P.fromTypedValue("", ja)
  }

  override def getType: String = WTypes.ARRAY

  // replace ${e} with value
  def template(s: String)(implicit ctx: ECtx) = {

    EESnakk.prepStr2(s, Nil)
  }

}


