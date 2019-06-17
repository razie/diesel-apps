package razie.diesel.dom

import mod.diesel.model.exec.EESnakk
import org.json.JSONObject
import razie.{clog, js}
import razie.diesel.dom.RDOM.{P, PValue}
import razie.diesel.exec.EEFunc
import razie.diesel.ext.CanHtml
import razie.wiki.parser.SimpleExprParser
import scala.collection.mutable
import scala.util.Try
import scala.util.parsing.json.JSONArray

//------------ expressions and conditions

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

/** arithmetic expressions */
case class AExpr2(a: Expr, op: String, b: Expr) extends Expr {
  val expr = (a.toDsl + op + b.toDsl)

  override def apply(v: Any)(implicit ctx: ECtx) =
    Some(applyTyped(v)).map(p => p.value.map(_.value).getOrElse(p.dflt)).get

  /** apply this function to an input value and a context */
  override def applyTyped(v: Any)(implicit ctx: ECtx): P = { //Try {

    // resolve an expression to P with value and type
    def top(x: Expr): Option[P] = x match {
      case CExpr(aa, tt)   => Some(P.fromTypedValue("", aa, tt))
      case aei:AExprIdent  => aei.tryApplyTyped(v)
      case _               => Some(P("", P.asString(a(v))))
    }

    def isNum(x: Expr): Boolean = x match {
      case CExpr(_, WTypes.NUMBER) => true
      case aei:AExprIdent          => aei.tryApplyTyped("").exists(_.ttype == WTypes.NUMBER)
      case _                       => false
    }

    val res: PValue[_] = op match {
      case "*" => {
        (a, b) match {
          case _ if isNum(a) && isNum(b) => {
            val as = a(v).toString
            if (as.contains(".")) {
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
            PValue("ERR:* on non numbers")
            throw new DieselExprException(
              s"ERR: multiply needs both numbers but got: ${a.getType} and ${b.getType} "
            )
          }
        }
      }

      case "+" => {
        (a, b) match {
          // json exprs are different, like cart + { item:...}
          case (aei:AExprIdent, JBlockExpr(jb))
            if aei.tryApplyTyped("").exists(_.ttype == WTypes.JSON) =>
            PValue(jsonExpr(op, a(v).toString, b(v).toString), WTypes.JSON)

          // json exprs are different, like cart + { item:...}
          case (aei:AExprIdent, bei:AExprIdent)
            if aei.tryApplyTyped("").exists(_.ttype == WTypes.JSON) &&
                bei.tryApplyTyped("").exists(_.ttype == WTypes.JSON) =>
            PValue(jsonExpr(op, a(v).toString, b(v).toString), WTypes.JSON)

          case _ if isNum(a) && isNum (b) => {
            // if a is num, b will be converted to num
            val as = a(v).toString
            if (as.contains(".")) {
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
            val av = a.applyTyped(v)
            val bv = b.applyTyped(v)

            // concat lists
            if(bv.ttype == WTypes.ARRAY ||
               av.ttype == WTypes.ARRAY) {
              val al = if(av.ttype == WTypes.ARRAY) av.calculatedTypedValue.asArray else List(av.calculatedTypedValue.value)
              val bl = if(bv.ttype == WTypes.ARRAY) bv.calculatedTypedValue.asArray else List(bv.calculatedTypedValue.value)
              val res = al ::: bl
              PValue(res, WTypes.ARRAY)
           } else  if(bv.ttype == WTypes.JSON ||
                  av.ttype == WTypes.JSON) {
              // json exprs are different, like cart + { item:...}
              PValue(jsonExpr(op, a(v).toString, b(v).toString), WTypes.JSON)
            } else {
              PValue(av.calculatedValue + bv.calculatedValue)
            }
          }
        }
      }

      case "-" => {
        (a, b) match {
          case _ if isNum(a) && isNum (b) => {
            // if a is num, b will be converted to num
            val as = a(v).toString
            if (as.contains(".")) {
              val ai = as.toFloat
              val bi = b(v).toString.toFloat
              PValue(ai - bi, WTypes.NUMBER)
            } else {
              val ai = as.toInt
              val bi = b(v).toString.toInt
              PValue(ai - bi, WTypes.NUMBER)
            }
          }

          case _ => {
            PValue("[ERR can't apply operator " + op + s" to ${a.getType} and ${b.getType}]")
          }
        }
      }

      // like in JS, if first not exist, use second
      case "||" if a.isInstanceOf[AExprIdent] => {
        a match {
          case aei:AExprIdent =>
            aei.tryApplyTyped("")
              .map(_.calculatedTypedValue)
              .getOrElse(
                b.applyTyped(v).calculatedTypedValue
              )

          case _ => {
            PValue("")
          }
        }
      }

      case _ => PValue("[ERR unknown operator " + op + "]")
    }

    P("", res.asString, res.contentType).copy(value = Some(res))
  }

  /** process a js operation like obja + objb */
  // todo decide if we should do this
  // todo this is not tested
  def jsonExprNEW(op: String, aa: String, bb: String) = {
//    val ai = new JSONObject (aa)
    val bi = new JSONObject(bb)
    val res = new JSONObject (aa)

//    ai.foreach { t =>
//      res.put(t._1, t._2)
//    }

    bi.keySet.toArray.foreach { kk =>
      val k = kk.toString
      val bv = bi.get(k)
      if (res.has(k)) {
        val ax = res.get(k)
        ax match {
          case al: JSONArray => {
            bv match {
              case bll:JSONArray =>
                res.put(k, JSONArray(al.list ::: bll.list))
              case _            =>
                res.put(k, JSONArray(al.list ::: bv :: Nil))
            }
          }
          case m: JSONObject => {
            val mres = new JSONObject(m, m.keySet.toArray(Array[String]()))
//            m.foreach { t =>
//              mres.put(t._1.toString, t._2)
//            }
            res.put(k, mres)
          }
          case y @ _ => res.put(k, y.toString + bv.toString)
        }
      } else res.put(k, bv)
    }
//    razie.js.tojsons(res.toMap)
    res.toString
  }

  /** process a js operation like obja + objb */
  def jsonExpr(op: String, aa: String, bb: String) = {
    val ai = razie.js.parse(aa)
    val bi = razie.js.parse(bb)
    val res = new mutable.HashMap[String, Any]()

    ai.foreach { t =>
      res.put(t._1, t._2)
    }

    bi.foreach { t =>
      val k = t._1
      val bv = t._2
      if (res.contains(k)) {
        val ax = res(k)
        ax match {

          case al: List[_] => {
            bv match {
                // add lists
              case bll: List[_] => res.put(k, al ::: bll)
              case _            => res.put(k, al ::: bv :: Nil)
            }
          }

          case m: Map[_, _] => {
            // merge maps
            val mres = new mutable.HashMap[String, Any]()
            m.foreach { t =>
              mres.put(t._1.toString, t._2)
            }
            res.put(k, mres)
          }

          case y @ _ => {
            (y, bv) match {
//              case (a:Int, b:Int) => res.put(k, a+b)
              // todo this will concatenate strings instead of merging maps
              case _ => res.put(k, bv)
//              case _ => res.put(k, y.toString + bv.toString)
                // todo this will concatenate strings instead of merging maps
            }
          }
        }
      } else res.put(k, bv)
    }
    razie.js.tojsons(res.toMap)
  }

  override def getType = a.getType
}

/** a qualified identifier
  *
  * @param start qualified expr a.b.c - this is used in places as such... don't replace with just a
  * @param rest the rest from the first []
  */
case class AExprIdent(val start: String, rest:List[P] = Nil) extends Expr {
  def expr = start.toString + (
    if (rest.size > 0) rest.mkString("[", "][", "]")
    else ""
    )

  // allow ctx["name"] as well as name
  def getp(name: String)(implicit ctx: ECtx): Option[P] = {
//     if("ctx" == name)
    ctx.getp(name)
  }

  override def apply(v: Any)(implicit ctx: ECtx) = applyTyped(v).calculatedValue

  // don't blow up - used when has defaults
  def tryApplyTyped(v: Any)(implicit ctx: ECtx): Option[P] =
    getp(start).flatMap { startP =>
      rest.foldLeft(Option(startP))((a, b) => access(a, b, false))
    }
  // todo why do i make up a parm?

  override def applyTyped(v: Any)(implicit ctx: ECtx): P =
    getp(start).flatMap { startP =>
      rest.foldLeft(Option(startP))((a, b) => access(a, b, true))
    }.getOrElse(P(start, "", WTypes.UNDEFINED))
  // todo why do i make up a parm?

  def access(p: Option[P], accessor: P, blowUp: Boolean)(implicit ctx: ECtx): Option[P] = {
    // accessor - need to reset value, so we keep re-calculating it in context
    val av = accessor.copy(value=None).calculatedTypedValue
//    val av = accessor.calculatedTypedValue

    p.flatMap { p =>
      // based on the type of p
      val pv = p.calculatedTypedValue

      def throwIt: Option[P] = {
        if (blowUp)
          throw new DieselExprException(
            s"Cannot access $expr of type ${pv.contentType} with ${accessor} of type ${accessor.ttype}"
          )
        None
      }

      if (av.contentType == WTypes.NUMBER) {
        val ai = av.asInt

        pv.contentType match {

          case WTypes.ARRAY => {
            val list = pv.asArray
            val res =
              if (ai >= 0)
                list.apply(ai)
              else  // negative - subtract from length
                list.apply(list.size - ai)

            Some(P.fromTypedValue("", res))
          }

          case WTypes.STRING => {
            val ps = pv.asString

            if (blowUp && ps.length < ai)
              throw new DieselExprException(s"$ai out of bounds of String: $ps")

            val res =
              if (ai >= 0)
                ps.charAt(ai)
              else  // negative - subtract from length
                ps.charAt(ps.length - ai)

            Some(P.fromTypedValue("", res.toString))
          }

          case _ => throwIt
        }
      } else if (av.contentType == WTypes.STRING) {
        pv.contentType match {

          case WTypes.JSON =>
            val as = av.asString
            val map = pv.asJson

            val res = map.get(as)
            res match {
              case Some(v) =>
                Some(P.fromTypedValue("", v))

              case None =>
//                if (blowUp) throw new DieselExprException(s"$expr does not have field $accessor")
//                else
            None // field not found - undefined
            }

          case _ => throwIt
        }
      } else if (av.contentType == WTypes.RANGE) {
        // todo support reversing, if ai < zi
        var ai = av.asRange.start
        var zi = av.asRange.end

        pv.contentType match {

          case WTypes.ARRAY => {
            val list = pv.asArray

            if (ai < 0) ai = list.size + ai - 1
            if (zi < 0) zi = list.size + zi - 1
            if (zi == scala.Int.MaxValue) zi = list.size - 1

            if (blowUp && list.size < ai)
              throw new DieselExprException(s"$ai out of bounds of List")
            if (blowUp && list.size < zi)
              throw new DieselExprException(s"$zi out of bounds of List")

            val res = list.slice(ai, zi + 1)

            Some(P.fromTypedValue("", res))
          }

          case WTypes.STRING => {
            val ps = pv.asString

            if (ai < 0) ai = ps.length + ai - 1
            if (zi < 0) zi = ps.length + zi - 1
            if (zi == scala.Int.MaxValue) zi = ps.length - 1

            // todo trim string
            if (blowUp && ps.length < ai)
              throw new DieselExprException(s"$ai out of bounds of String: $ps")
            if (blowUp && ps.length < zi)
              throw new DieselExprException(s"$zi out of bounds of String: $ps")

            val res = ps.substring(ai, zi + 1)

            Some(P.fromTypedValue("", res.toString))
          }

          case _ => throwIt
        }
      } else if (av.contentType == WTypes.UNDEFINED) {
        // looking for some child of undefined
        Some(p)
      } else {
        throwIt
      }
    }
  }

  def toStringCalc (implicit ctx:ECtx) = {
    start + rest.map(_.calculatedValue).mkString("[", "][", "]")
//    rest.foldLeft(start)((a, b) => a + "." + b.calculatedValue)
  }
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

/** a function */
case class AExprFunc(val expr: String, parms: List[RDOM.P]) extends Expr {

  override def apply(v: Any)(implicit ctx: ECtx) = applyTyped(v).calculatedValue
  override def applyTyped(v: Any)(implicit ctx: ECtx): P = {
    expr match {
      case "sizeOf" => {
        parms.headOption
          .map { p =>
            val pv = p.calculatedTypedValue
            if (pv.contentType == WTypes.ARRAY) {
              val sz = pv.asArray.size
              P("", sz.toString, WTypes.NUMBER).withValue(sz, WTypes.NUMBER)
            } else {
              throw new DieselExprException(
                "Not array: " + p.name + " is:" + pv.toString
              )
            }
          }
          .getOrElse(
            throw new DieselExprException("No arguments for sizeOf")
          )
      }

      case _ =>
        throw new DieselExprException("Function not found: " + expr)
    }

  }

  override def toDsl = expr + "()"
  override def toHtml = tokenValue(toDsl)
}

/** an xpath expr */
case class XPathIdent(val expr: String) extends Expr {
  override def apply(v: Any)(implicit ctx: ECtx) = ctx.apply(expr)
  override def applyTyped(v: Any)(implicit ctx: ECtx): P = ???
}

/**
  * constant expression - similar to PValue
  */
case class CExpr[T](ee: T, ttype: String = "") extends Expr {
  val expr = ee.toString

  override def apply(v: Any)(implicit ctx: ECtx) = applyTyped(v).dflt

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
        P("", es, ttype).withValue(ee, ttype)
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

  override def apply(v: Any)(implicit ctx: ECtx) = applyTyped(v).dflt

  override def applyTyped(v: Any)(implicit ctx: ECtx) = {
//    val orig = template(expr)
    val orig = ex
      .map(t=> (t._1, t._2.applyTyped(v)))
      .map(t=> (t._1, t._2 match {
        case p@P(n,d,WTypes.NUMBER, _, _, _, Some(PValue(i:Int, _))) => i
        case p@P(n,d,WTypes.NUMBER, _, _, _, Some(PValue(i:Double, _))) => i

        case p:P => p.dflt match {
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
          case "~=" | "like" => a(in).toString matches b(in).toString
          case "<="          => a(in).toString <= b(in).toString
          case ">="          => a(in).toString >= b(in).toString
          case "<"           => a(in).toString < b(in).toString
          case ">"           => a(in).toString > b(in).toString

          case "contains"    => a(in).toString contains b(in).toString
          case "containsNot" => !(a(in).toString contains b(in).toString)

          // THESE CANNOT CHANGE...

          case "is"  if b.toString == "null" => ap.ttype == WTypes.UNDEFINED
          case "not" if b.toString == "null" => ap.ttype != WTypes.UNDEFINED
          case "is"  if b.toString == "defined" => ap.ttype != WTypes.UNDEFINED
          case "not" if b.toString == "defined" => ap.ttype == WTypes.UNDEFINED //as.length <= 0

          case "is"  if b.toString == "nzlen" => ap.ttype != WTypes.UNDEFINED && as.length > 0
          case "not" if b.toString == "nzlen" => ap.ttype == WTypes.UNDEFINED || as.length <= 0

          case "is"  if b.toString == "empty" =>
              if (ap.calculatedTypedValue.contentType == WTypes.JSON)
                 ap.calculatedTypedValue.asJson.isEmpty
              else
                /*ap.ttype != WTypes.UNDEFINED &&*/ as.length == 0

          case "not" if b.toString == "empty" => as.length > 0

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
            throw new DieselExprException(
              "Found :number expected :boolean"
            )
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
