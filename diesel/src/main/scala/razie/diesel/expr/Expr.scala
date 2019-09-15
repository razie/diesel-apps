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

// exprs

/** boolean expressions */
abstract class BExpr(e: String) extends HasDsl {
  def apply(e: Any)(implicit ctx: ECtx): Boolean

  override def toDsl = e
}

/** boolean expression block - show the () when printing */
case class BExprBlock(a: BExpr) extends BExpr("") {
  override def apply(e: Any)(implicit ctx: ECtx) = a.apply(e)
  override def toDsl = "(" + a.toDsl + ")"
}

/** negated boolean expression */
case class BCMPNot(a: BExpr) extends BExpr("") {
  override def apply(e: Any)(implicit ctx: ECtx) = !a.apply(e)
  override def toDsl = "NOT (" + a.toDsl + ")"
}

/** const boolean expression */
case class BCMPConst(a: String) extends BExpr(a) {
  override def apply(e: Any)(implicit ctx: ECtx) = a == "true"
}

/** composed boolean expression */
case class BCMP1(a: BExpr, op: String, b: BExpr)
    extends BExpr(a.toDsl + " " + op + " " + b.toDsl) {
  override def apply(in: Any)(implicit ctx: ECtx) = op match {
    case "||" | "or"  => a.apply(in) || b.apply(in)
    case "&&" | "and" => a.apply(in) && b.apply(in)
    case _ => {
      clog << "[ERR Operator " + op + " UNKNOWN!!!]"; false
    }
  }

  override def toString = a.toString + " " + op + " " + b.toString
}

/** simple boolean expression */
case class BCMP2(a: Expr, op: String, b: Expr)
    extends BExpr(a.toDsl + " " + op + " " + b.toDsl) {

  override def apply(in: Any)(implicit ctx: ECtx): Boolean = {
    (a, b) match {
      case (CExpr(aa, WTypes.NUMBER), CExpr(bb, WTypes.NUMBER)) => {
        val as = aa.toString
        val bs = bb.toString

        cmpNums(as, bs, op)
      }

      case _ => {
        lazy val ap = a.applyTyped(in)
        lazy val bp = b.applyTyped(in)
        lazy val as = ap.calculatedValue

        def b_is(s: String) =
          b.isInstanceOf[AExprIdent] && s == b
            .asInstanceOf[AExprIdent]
            .expr
            .toLowerCase

        def isNum(p:P) = {
          p.ttype == WTypes.NUMBER || p.value.exists(_.contentType == WTypes.NUMBER)
        }

        val cmpop = op match {
          case "?=" | "==" | "!=" | "~=" | "like" | "<=" | ">=" | "<" | ">" => true
          case _ => false
        }

        // if one of them is number, don't care about the other... could be a string containing a num...
        if (cmpop && (isNum(ap) || isNum(bp))) {
          return cmpNums(ap.calculatedValue, bp.calculatedValue, op)
        }

        op match {
          case "?="          => a(in).toString.length >= 0 // anything with a default
          case "!="          => a(in) != b(in)
          case "~=" | "matches" => a(in).toString matches b(in).toString
          case "<="          => a(in).toString <= b(in).toString
          case ">="          => a(in).toString >= b(in).toString
          case "<"           => a(in).toString < b(in).toString
          case ">"           => a(in).toString > b(in).toString

          case "contains"    =>   a(in).toString contains b(in).toString
          case "containsNot" => !(a(in).toString contains b(in).toString)

          // THESE CANNOT CHANGE...

          case "is"  if b.toString == "null" => ap.ttype == WTypes.UNDEFINED
          case "not" if b.toString == "null" => ap.ttype != WTypes.UNDEFINED
          case "is"  if b.toString == "defined" => ap.ttype != WTypes.UNDEFINED
          case "not" if b.toString == "defined" => ap.ttype == WTypes.UNDEFINED //as.length <= 0

          case "is"  if b.toString == "nzlen" => ap.ttype != WTypes.UNDEFINED && as.length > 0 && as.trim != "null"
          case "not" if b.toString == "nzlen" => ap.ttype == WTypes.UNDEFINED || as.length <= 0 || as.trim == "null"

          case "is"  if b.toString == "empty" =>
            if (ap.calculatedTypedValue.contentType == WTypes.JSON)
               ap.calculatedTypedValue.asJson.isEmpty
            else if (ap.calculatedTypedValue.contentType == WTypes.ARRAY)
               ap.calculatedTypedValue.asArray.isEmpty
            else
              /*ap.ttype != WTypes.UNDEFINED &&*/ as.length == 0

          case "not" if b.toString == "empty" =>
            if (ap.calculatedTypedValue.contentType == WTypes.JSON)
              !ap.calculatedTypedValue.asJson.isEmpty
            else if (ap.calculatedTypedValue.contentType == WTypes.ARRAY)
              !ap.calculatedTypedValue.asArray.isEmpty
            else
            /*ap.ttype != WTypes.UNDEFINED &&*/ as.length != 0

          case "is"  if b.toString == "undefined" => ap.ttype == WTypes.UNDEFINED
          case "not" if b.toString == "undefined" => ap.ttype != WTypes.UNDEFINED

          case "is" if b_is("number") => as.matches("[0-9.]+")

          case "is" if b_is("boolean") =>
            a.getType == WTypes.BOOLEAN || ap.calculatedTypedValue.contentType == WTypes.BOOLEAN

          case "is" if b_is("json") || b_is("object") =>
            ap.calculatedTypedValue.contentType == WTypes.JSON

          case "is" if b_is("array") => {
            val av = ap.calculatedTypedValue
            av.contentType == WTypes.ARRAY
          }

          case "is" | "==" if bp.ttype == WTypes.ARRAY || ap.ttype == WTypes.ARRAY => {
            val av = ap.calculatedTypedValue
            val bv = bp.calculatedTypedValue
            val al = av.asArray
            val bl = bv.asArray

            if(al.size != bl.size) {
              false
            } else {
              al.zip(bl).foldLeft(true)((a,b) => a && (b._1 == b._2))
            }
          }

          case "is" => { // is nuber or is date or is string etc
            /* x is TYPE */
            if (b.isInstanceOf[AExprIdent])
              (
                /* x is TYPE */
                a.getType.toLowerCase == b
                  .asInstanceOf[AExprIdent]
                  .expr
                  .toLowerCase ||

                /* x is string */
                //                "string" == b.asInstanceOf[AExprIdent].expr.toLowerCase &&
                //                  a.getType.length == 0 ||

                /** just evaluate b */
                (as equals bp.calculatedValue)
              )
            else
              /* if type expr not known, then behave like equals */
              (as == b(in).toString)
          }


          // also should be
          // todo why also should be ???
          case "not" => a(in).toString.length > 0 && a(in) != b(in)

          case "=="          => a(in) == b(in)

          case _ if op.trim == "" => {
            // no op - look for boolean parms?
            ap.ttype == WTypes.BOOLEAN && "true" == a(in).toString
          }

          case _ => {
            clog << "[ERR Operator " + op + " UNKNOWN!!!]";
            false
          }
        }
      }
    }
  }

  private def cmpNums(as: String, bs: String, op: String): Boolean = {
    Try {
      val ai = {
        if (as.contains(".")) as.toDouble
        else as.toInt
      }
      val bi = {
        if (bs.contains(".")) bs.toDouble
        else bs.toInt
      }

    op match {
      case "?="  => true
      case "=="  => ai == bi
      case "!="  => ai != bi
      case "<="  => ai <= bi
      case ">="  => ai >= bi
      case "<"   => ai < bi
      case ">"   => ai > bi
      case "is"  => ai == bi
      case "not" => ai != bi
      case _ => {
        clog << "[ERR Operator " + op + " UNKNOWN!!!]";
        false
      }
    }
    } getOrElse (false)
  }
}

/** single term bool expression */
case class BCMPSingle(a: Expr) extends BExpr(a.toDsl) {

  def toBoolean(in: P)(implicit ctx: ECtx) = {
    a.getType match {

      case WTypes.NUMBER => {
        throw new DieselExprException("Found :number expected :boolean")
      }

      case WTypes.BOOLEAN => {
        "true" == in.currentStringValue
      }

      case _ if "true" == in.currentStringValue => {
        true
      }

      case _ if "false" == in.currentStringValue => {
        false
      }

      case s @ _ => {
        // it was some parameter that apply() evaluated

        in.ttype match {

          case WTypes.NUMBER => {
            throw new DieselExprException(
              "Found :number expected :boolean"
            )
          }

          case WTypes.BOOLEAN => {
            "true" == in.currentStringValue
          }

          case _ if "true" == in.currentStringValue => {
            true
          }

          case _ if "false" == in.currentStringValue => {
            false
          }

          case WTypes.UNDEFINED => {
            false // todo is this cocher?
          }

          case s @ _ => {
            val t = if (s.length > 0) s else ":unknown"
            clog << (s"Found $t expected :boolean")
            throw new DieselExprException(
              s"Found $t expected :boolean details: ($a)"
            )
          }
        }
      }
    }
  }

  override def apply(in: Any)(implicit ctx: ECtx) = {
    val ap = a.applyTyped(in)
    toBoolean(ap)
  }
}

/** just a constant expr */
object BExprFALSE extends BExpr("FALSE") {
  def apply(e: Any)(implicit ctx: ECtx): Boolean = false

  override def toDsl = "FALSE"
}
