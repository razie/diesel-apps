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
      case CExpr(aa, tt) => Some(P ("", aa, tt))
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
            ctx.getp(aid).map(_.calculatedTypedValue).getOrElse(PValue(b(v).toString))

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

    P("", res.toString, res.contentType).copy(value = Some(res))
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

/** a qualified identifier */
case class XPathIdent (val expr:String) extends Expr {
  override def apply (v:Any)(implicit ctx:ECtx) = ctx.apply(expr)
}

/** constant expression
  *
  *  TODO i'm loosing the type definition
  */
case class CExpr (ee : String, ttype:String="") extends Expr {
  val expr = ee.toString

  override def apply (v:Any)(implicit ctx:ECtx) = applyTyped(v).dflt

  override def applyTyped (v:Any)(implicit ctx:ECtx) : P = {
    if (ttype == WTypes.NUMBER) {
      if (ee.contains("."))
        P("", ee, ttype).withValue(ee.toDouble)
      else
        P("", ee, ttype).withValue(ee.toInt)
    } else if (ttype == WTypes.BOOLEAN) {
      P("", ee, ttype).withValue(ee.toBoolean)
    } else {
      // expand templates by default
      if (ee contains "${") {
        val PAT = """\$\{([^\}]*)\}""".r
        val eeEscaped = ee.replaceAllLiterally("(", """\(""").replaceAllLiterally(")", """\)""")
        val s1 = PAT.replaceAllIn(ee, { m =>
          (new SimpleExprParser).parseExpr(m.group(1)).map { e =>
            P("x", "", "", "", "", Some(e)).calculatedValue
          } getOrElse
            s"{ERROR: ${m.group(1)}"
        })
        P("", s1, ttype)
      } else
        P("", ee, ttype)
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

/** negated boolean expression */
case class BCMPNot(a: BExpr) extends BExpr("") {
  override def apply(e: Any)(implicit ctx: ECtx) = !a.apply(e)
}

/** const boolean expression */
case class BCMPConst(a: String) extends BExpr(a) {
  override def apply(e: Any)(implicit ctx: ECtx) = a == "true"
}

/** composed boolean expression */
case class BCMP1(a: BExpr, op: String, b: BExpr) extends BExpr(a.toDsl + " " + op + " " + b.toDsl) {
  override def apply(in: Any)(implicit ctx: ECtx) = op match {
    case "||" => a.apply(in) || b.apply(in)
    case "&&" => a.apply(in) && b.apply(in)
    case _ => {
      clog << "[ERR Operator " + op + " UNKNOWN!!!]"; false
    }
  }

  override def toString = a.toString + " " + op + " " + b.toString
}

/** simple boolean expression */
case class BCMP2(a: Expr, op: String, b: Expr) extends BExpr(a.toDsl + " " + op + " " + b.toDsl) {
  override def apply(in: Any)(implicit ctx: ECtx) = {
    (a, b) match {
      case (CExpr(aa, WTypes.NUMBER), CExpr(bb, WTypes.NUMBER)) => {
        val ai = {
          if(aa.contains(".")) aa.toDouble
          else aa.toInt
        }
        val bi = {
          if(bb.contains(".")) bb.toDouble
          else bb.toInt
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

      case _ => {
        def ap = a.applyTyped(in)
        def bp = b.applyTyped(in)
        def as = ap.calculatedValue
        def b_is (s:String) = b.isInstanceOf[AExprIdent] && s == b.asInstanceOf[AExprIdent].expr.toLowerCase

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
          case "is" if b_is("boolean") => a.getType == WTypes.BOOLEAN

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
          case "not" if b.toString == "empty" => a(in).toString.length > 0

            // also should be
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
}

/** just a constant expr */
object BExprFALSE extends BExpr ("FALSE") {
  def apply(e: Any)(implicit ctx: ECtx): Boolean = false

  override def toDsl = "FALSE"
}


