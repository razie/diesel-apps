package razie.diesel.dom

import mod.diesel.model.exec.EESnakk
import razie.clog
import razie.diesel.dom.RDOM.{P, PValue}
import razie.diesel.exec.EEFunc
import razie.diesel.ext.CanHtml
import razie.wiki.parser.SimpleExprParser

import scala.collection.mutable
import scala.util.Try

//------------ expressions and conditions

/** deserialization is assumed via DSL
  *
  *  the idea is that all activities would have an external DSL form as well
  *  and can serialize themselves in that form
  *
  *  serialize the DEFINITION only - not including states/values
  */
trait HasDsl /*extends GReferenceable*/ {
  def serialize : String = toDsl

  /** serialize the DEFINITION - not including
    */
  def toDsl : String
  def toIndentedDsl (indent:Int=>String, level:Int) = indent(level) + toDsl
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
  def apply (v:Any)(implicit ctx:ECtx) : Any

  /** apply this function to an input value and a context */
  def applyTyped (v:Any)(implicit ctx:ECtx) : P = {
    val res = apply(v)
    // todo use the PValue
    P("", res.toString, getType)
  }

  /** what is the resulting type - when known */
  def getType : String = ""
}

/** an expression */
abstract class Expr extends WFunc with HasDsl with CanHtml {
  def expr : String
  override def toString = toDsl
  override def toDsl = expr
  override def toHtml = tokenValue(toDsl)
}

/** arithmetic expressions */
case class AExpr2 (a:Expr, op:String, b:Expr) extends Expr {
  val expr = (a.toDsl + op + b.toDsl)

  override def apply(v: Any)(implicit ctx: ECtx) = Some(applyTyped(v)).map(p=> p.value.map(_.value).getOrElse(p.dflt)).get

  /** apply this function to an input value and a context */
  override def applyTyped(v: Any)(implicit ctx: ECtx): P = { //Try {

    // resolve an expression to P with value and type
    def top (x:Expr) : Option[P] = x match {
      case CExpr(aa, tt) => Some(P ("", aa.toString, tt).withValue(aa, tt))
      case AExprIdent(aid) => ctx.getp(aid)
      case _ => Some(P("", a(v).toString))
    }

    def isNum (x:Expr) : Boolean = x match {
      case CExpr(_, WTypes.NUMBER) => true
      case AExprIdent(aid) => ctx.getp(aid).exists(_.ttype == WTypes.NUMBER)
      case _ => false
    }

    val res:PValue[_] = op match {
      case "*" => {
        (a, b) match {
          case _ if isNum(a) && isNum(b) => {
            val as = a(v).toString
            if(as.contains(".")) {
              val ai = as.toFloat
              val bi = b(v).toString.toFloat
              PValue(ai * bi, WTypes.NUMBER)
            } else {
              val ai = as.toInt
              val bi = b(v).toString.toInt
              PValue(ai * bi, WTypes.NUMBER)
            }
            // todo float and type safe numb
          }

          case _ => {
            PValue("???")
          }
        }
      }

      case "+" => {
        (a, b) match {
            // json exprs are different, like cart + { item:...}
          case (AExprIdent(aid), JBlockExpr(jb)) if ctx.getp(aid).exists(_.ttype == WTypes.JSON) =>
            PValue(jsonExpr(op, a(v).toString, b(v).toString), WTypes.JSON)

          // json exprs are different, like cart + { item:...}
          case (AExprIdent(aid), AExprIdent(bid)) if
            ctx.getp(aid).exists(_.ttype == WTypes.JSON) &&
            ctx.getp(bid).exists(_.ttype == WTypes.JSON) =>

            PValue(jsonExpr(op, a(v).toString, b(v).toString), WTypes.JSON)

          case _ if isNum(a) => {
            // if a is num, b will be converted to num
            val as = a(v).toString
            if(as.contains(".")) {
              val ai = as.toFloat
              val bi = b(v).toString.toFloat
              PValue(ai + bi, WTypes.NUMBER)
            } else {
              val ai = as.toInt
              val bi = b(v).toString.toInt
              PValue(ai + bi, WTypes.NUMBER)
            }
          }

          case _ => {
            PValue(a(v).toString + b(v).toString)
          }
        }
      }

        // WTF is this?
      case "||" if a.isInstanceOf[AExprIdent] => {
        a match {
          case AExprIdent(aid) =>
            ctx.getp(aid).map(_.calculatedTypedValue).getOrElse(
              // had type issues with this:
              // PValue(b(v).toString)

              // this makes more sense
              b.applyTyped(v).calculatedTypedValue
            )

          case _ => {
            PValue("")
          }
        }
      }

      case _ => PValue("[ERR unknown operator " + op + "]")
    }
//  }.recover {
//    case t:Throwable => {
//      clog << t
//      t.toString
//    }
//  }.get

    P("", res.value.toString, res.contentType).copy(value = Some(res))
  }

  /** process a js operation like obja + objb */
  def jsonExpr (op:String, aa:String, bb:String) = {
    val ai = razie.js.parse(aa)
    val bi = razie.js.parse(bb)
    val res = new mutable.HashMap[String,Any]()
    ai.foreach{t => res.put(t._1, t._2)}
    bi.foreach{t =>
      val k = t._1
      val bv = t._2
      if(res.contains(k)) {
        val ax = res(k)
        ax match {
          case al : List[_] => {
            bv match {
              case bll : List[_] => res.put(k, al ::: bll)
              case _ => res.put(k, al ::: bv :: Nil)
            }
          }
          case m : Map[_, _] => {
            val mres = new mutable.HashMap[String,Any]()
            m.foreach{t => mres.put(t._1.toString,t._2)}
            res.put(k,mres)
          }
          case y @ _ => res.put(k, y.toString + bv.toString)
        }
      } else res.put(k,bv)
    }
    razie.js.tojsons(res.toMap)
  }

  override def getType = a.getType
}

/** a qualified identifier */
case class AExprIdent (val expr:String) extends Expr {
  override def apply (v:Any)(implicit ctx:ECtx) = ctx.apply(expr)

  override def applyTyped (v:Any)(implicit ctx:ECtx) : P = ctx.getp(expr).getOrElse(P(expr, ""))
}

/** a function */
case class AExprFunc (val expr:String, parms:List[RDOM.P]) extends Expr {

  override def apply (v:Any)(implicit ctx:ECtx) = applyTyped(v).calculatedValue
  override def applyTyped (v:Any)(implicit ctx:ECtx) : P = {
    expr match {
      case "sizeOf" => {
        parms.headOption.map {p=>
          val pv = p.calculatedTypedValue
          if(pv.contentType == WTypes.ARRAY) {
            val sz = pv.value.asInstanceOf[List[_]].size
            P("", sz.toString, WTypes.NUMBER).withValue(sz, WTypes.NUMBER)
          } else {
            throw new IllegalArgumentException ("Not array: " + p.name + " is:" + pv.toString)
          }
        }.getOrElse(
          throw new IllegalArgumentException ("No arguments for sizeOf")
        )
      }

      case _ => throw new IllegalArgumentException ("Function not found: " + expr)
    }

  }

  override def toDsl = expr + "()"
  override def toHtml = tokenValue(toDsl)
}

/** a qualified identifier */
case class XPathIdent (val expr:String) extends Expr {
  override def apply (v:Any)(implicit ctx:ECtx) = ctx.apply(expr)
}

/**
  * constant expression - similar to PValue
  */
case class CExpr[T] (ee : T, ttype:String="") extends Expr {
  val expr = ee.toString

  override def apply (v:Any)(implicit ctx:ECtx) = applyTyped(v).dflt

  override def applyTyped (v:Any)(implicit ctx:ECtx) : P = {
    val es = ee.toString

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
          val eeEscaped = es.replaceAllLiterally("(", """\(""").replaceAllLiterally(")", """\)""")
          s1 = PAT.replaceAllIn(es, { m =>
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
          case e : Exception => throw new IllegalArgumentException(s"REGEX err for $es ").initCause(e)
        }
        P("", s1, ttype)
      } else
        P("", es, ttype).withValue(ee, ttype)
    }
  }

  override def toDsl = if(ttype == "String") ("\"" + expr + "\"") else expr
  override def getType: String = ttype
  override def toHtml = tokenValue(toDsl)
}


/** arithmetic expressions */
case class AExpr (val expr : String) extends Expr {
  override def apply (v:Any)(implicit ctx:ECtx) = v
}

/** a block */
case class BlockExpr (ex : Expr) extends Expr {
  val expr = "( " + ex.toString + " )"
  override def apply (v:Any)(implicit ctx:ECtx) = ex.apply(v)
  override def getType: String = ex.getType
}

/** a js expression
  * js:a.b
  * js:{...}
  */
case class JSSExpr (s : String) extends Expr {
  val expr = "js{{ " + s + " }}"

  override def getType: String = WTypes.UNKNOWN

  override def apply (v:Any)(implicit ctx:ECtx) =
    EEFunc.execute (s) //.dflt

  override def applyTyped (v:Any)(implicit ctx:ECtx) : P = {
    EEFunc.executeTyped (s)
  }
}

/** a scala expression
  * sc:a.b
  * sc:{...}
  */
case class SCExpr (s : String) extends Expr {
  val expr = "sc{{ " + s + " }}"

  override def getType: String = WTypes.UNKNOWN

  override def apply (v:Any)(implicit ctx:ECtx) = ???

  override def applyTyped (v:Any)(implicit ctx:ECtx) : P = ???
}

/** a json block */
case class JBlockExpr (ex : String) extends Expr {
  val expr = "{ " + ex.toString + " }"

  override def apply (v:Any)(implicit ctx:ECtx) = template(expr)

  override def getType: String = WTypes.JSON

  // replace ${e} with value
  def template (s:String)(implicit ctx:ECtx) = {

    EESnakk.prepStr2(s, Nil)

//    val PATT = """(\$\w+)""".r
//    val u = PATT.replaceSomeIn(s, { m =>
//      val n = if(m.matched.length > 0) m.matched.substring(1) else ""
//      ctx.get(n).map(x=>
//        razie.diesel.ext.stripQuotes(x)
//      )
//    })
//    u
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
case class BCMP1(a: BExpr, op: String, b: BExpr) extends BExpr(a.toDsl + " " + op + " " + b.toDsl) {
  override def apply(in: Any)(implicit ctx: ECtx) = op match {
    case "||" | "or" => a.apply(in) || b.apply(in)
    case "&&" | "and" => a.apply(in) && b.apply(in)
    case _ => {
      clog << "[ERR Operator " + op + " UNKNOWN!!!]"; false
    }
  }

  override def toString = a.toString + " " + op + " " + b.toString
}

/** simple boolean expression */
case class BCMP2(a: Expr, op: String, b: Expr) extends BExpr(a.toDsl + " " + op + " " + b.toDsl) {

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

        def b_is (s:String) = b.isInstanceOf[AExprIdent] && s == b.asInstanceOf[AExprIdent].expr.toLowerCase

        if (ap.ttype == WTypes.NUMBER && bp.ttype == WTypes.NUMBER) {
          return cmpNums(ap.calculatedValue, bp.calculatedValue, op)
        }

        op match {
          case "?=" => a(in).toString.length >= 0 // anything with a default
          case "==" => a(in) == b(in)
          case "!=" => a(in) != b(in)
          case "~=" | "like" => a(in).toString matches b(in).toString
          case "<=" => a(in).toString <= b(in).toString
          case ">=" => a(in).toString >= b(in).toString
          case "<" => a(in).toString < b(in).toString
          case ">" => a(in).toString > b(in).toString

          case "contains" => a(in).toString contains b(in).toString
          case "containsNot" => !(a(in).toString contains b(in).toString)

          case "is" if b.toString == "defined" => as.length > 0

          case "is" if b.toString == "empty" || b.toString == "undefined" => as.length == 0

          case "is" if b_is("number")  => as.matches("[0-9.]+")
          case "is" if b_is("boolean") => a.getType == WTypes.BOOLEAN || ap.calculatedTypedValue.contentType == WTypes.BOOLEAN
          case "is" if b_is("json") => ap.calculatedTypedValue.contentType == WTypes.JSON

          case "is" if b_is("array") => {
            val av = ap.calculatedTypedValue
            av.contentType == WTypes.ARRAY
          }

          case "is" => { // is nuber or is date or is string etc
            /* x is TYPE */
            if(b.isInstanceOf[AExprIdent]) (
              /* x is TYPE */
              a.getType.toLowerCase == b.asInstanceOf[AExprIdent].expr.toLowerCase ||

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

          case "not" if b.toString == "defined" => a(in).toString.length <= 0
          case "not" if b.toString == "empty" || b.toString == "undefined" => a(in).toString.length > 0

          // also should be
          // todo why also should be ???
          case "not" => a(in).toString.length > 0 && a(in) != b(in)

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

  private def cmpNums (as:String, bs:String, op:String): Boolean = {
    val ai = {
      if(as.contains(".")) as.toDouble
      else as.toInt
    }
    val bi = {
      if(bs.contains(".")) bs.toDouble
      else bs.toInt
    }

    op match {
      case "?=" => true
      case "==" => ai == bi
      case "!=" => ai != bi
      case "<=" => ai <= bi
      case ">=" => ai >= bi
      case "<" => ai < bi
      case ">" => ai > bi
      case "is" => ai == bi
      case "not" => ai != bi
      case _ => {
        clog << "[ERR Operator " + op + " UNKNOWN!!!]";
        false
      }
    }
  }
}

/** single term bool expression */
case class BCMPSingle(a: Expr) extends BExpr(a.toDsl) {

  def toBoolean(in: P)(implicit ctx: ECtx) = {
    a.getType match {

      case WTypes.NUMBER => {
        throw new IllegalArgumentException("Found :number expected :boolean")
      }

      case WTypes.BOOLEAN => {
        "true" == in.dflt
      }

      case _ if "true" == in.dflt => {
        true
      }

      case _ if "false" == in.dflt => {
        false
      }

      case s @ _ => {
        // it was some parameter that apply() evaluated

        in.ttype match {

          case WTypes.NUMBER => {
            throw new IllegalArgumentException("Found :number expected :boolean")
          }

          case WTypes.BOOLEAN => {
            "true" == in.dflt
          }

          case _ if "true" == in.dflt => {
            true
          }

          case _ if "false" == in.dflt => {
            false
          }

          case s @ _ => {
            val t = if(s.length > 0) s else ":unknown"
            clog << (s"Found $t expected :boolean")
            throw new IllegalArgumentException(s"Found $t expected :boolean details: ($a)")
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
object BExprFALSE extends BExpr ("FALSE") {
  def apply(e: Any)(implicit ctx: ECtx): Boolean = false

  override def toDsl = "FALSE"
}


