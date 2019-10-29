/**   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.expr

import razie.diesel.dom.RDOM.P
import razie.diesel.dom._
import razie.diesel.exec.EEFunc
import razie.diesel.ext.CanHtml
import razie.wiki.parser.SimpleExprParser

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
  def getType: WType = WTypes.wt.EMPTY
}

/** an expression */
abstract class Expr extends WFunc with HasDsl with CanHtml {
  def expr: String
  override def toString = toDsl
  override def toDsl = expr
  override def toHtml = tokenValue(toDsl)
}


case class ExprRange(val start: Expr, val end: Option[Expr]) extends Expr {
  /** what is the resulting type - when known */
  override def getType: WType = WTypes.wt.RANGE
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
  override def getType: WType = WTypes.wt.UNDEFINED
  override def expr = "null"

  override def apply(v: Any)(implicit ctx: ECtx) = applyTyped(v).calculatedValue

  override def applyTyped(v: Any)(implicit ctx: ECtx): P = P("", "", WTypes.UNDEFINED)

  override def toDsl = expr
  override def toHtml = expr
}


/**
  * constant expression - similar to PValue
  */
case class CExpr[T](ee: T, ttype: WType = WTypes.wt.EMPTY) extends Expr {
  val expr = ee.toString

  override def apply(v: Any)(implicit ctx: ECtx) = applyTyped(v).currentStringValue

  override def applyTyped(v: Any)(implicit ctx: ECtx): P = {
    val es = P.asString(ee)

    if (ttype == WTypes.NUMBER) {
      if (es.contains("."))
        P("", es, ttype).withValue(es.toDouble, WTypes.wt.NUMBER)
      else
        P("", es, ttype).withValue(es.toInt, WTypes.wt.NUMBER)
    } else if (ttype == WTypes.BOOLEAN) {
      P("", es, ttype).withValue(es.toBoolean, WTypes.wt.BOOLEAN)
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
                val res = P("x", "", WTypes.wt.EMPTY, Some(e)).calculatedValue

                // if the parm's value contains $ it would not be expanded - that's enough, eh?
                // todo recursive expansions?
                var e1 = res
                    .replaceAllLiterally("\\", "\\\\") // escape escaped
                    .replaceAll("\\$", "\\\\\\$")
                // should not escape double quotes all the time !!
//                e1 = e1
//                    .replaceAll("^\"", "\\\\\"") // escape double quotes
//                e1 = e1
//                    .replaceAll("""([^\\])"""", "$1\\\\\"") // escape unescaped double quotes

                e1
              } getOrElse
                s"{ERROR: ${m.group(1)}"
          })
        } catch {
          case e: Exception =>
            throw new DieselExprException(s"REGEX err for $es - " + e.getMessage)
              .initCause(e)
        }
        P("", s1, ttype)
      } else
        P("", es, ttype).withCachedValue(ee, ttype, es)
    }
  }

  override def toDsl = if (ttype == "String") ("\"" + expr + "\"") else expr
  override def getType: WType = ttype
  override def toHtml = tokenValue(escapeHtml(toDsl))
}

/** a block */
case class BlockExpr(ex: Expr) extends Expr {
  val expr = "( " + ex.toString + " )"
  override def apply(v: Any)(implicit ctx: ECtx) = ex.apply(v)
  override def applyTyped(v: Any)(implicit ctx: ECtx): P = ex.applyTyped(v)
  override def getType: WType = ex.getType
}

/** a js expression
  * js:a.b
  * js:{...}
  */
case class JSSExpr(s: String) extends Expr {
  val expr = "js{{ " + s + " }}"

  override def getType: WType = WTypes.wt.UNKNOWN

  override def apply(v: Any)(implicit ctx: ECtx) =
    EEFunc.execute("js", s) //.dflt

  override def applyTyped(v: Any)(implicit ctx: ECtx): P = {
      EEFunc.executeTyped("js", s)
  }
}

/** a scala expression
  * sc:a.b
  * sc:{...}
  */
case class SCExpr(s: String) extends Expr {
  val expr = "sc{{ " + s + " }}"

  override def getType: WType = WTypes.wt.UNKNOWN

  override def apply(v: Any)(implicit ctx: ECtx) =
    EEFunc.execute("scala", s) //.dflt

  override def applyTyped(v: Any)(implicit ctx: ECtx): P =
    EEFunc.executeTyped("scala", s)
}


