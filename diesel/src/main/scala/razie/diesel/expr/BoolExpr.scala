/**
  *   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.expr

import razie.clog
import razie.diesel.dom.RDOM.P
import razie.diesel.dom._
import scala.util.Try


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
      clog << s"[ERR BoolOperator $op UNKNOWN!!!] as in $a $op $b"; false
    }
  }

  override def toString = a.toString + " " + op + " " + b.toString
}

/** simple boolean expression */
case class BCMP2(a: Expr, op: String, b: Expr)
    extends BExpr(a.toDsl + " " + op + " " + b.toDsl) {

  override def apply(in: Any)(implicit ctx: ECtx): Boolean = {
    try {
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

          def isNum(p: P) = {
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
            case "?=" => a(in).toString.length >= 0 // anything with a default
            case "!=" => a(in) != b(in)
            case "~=" | "matches" => a(in).toString matches b(in).toString
            case "<=" => a(in).toString <= b(in).toString
            case ">=" => a(in).toString >= b(in).toString
            case "<" => a(in).toString < b(in).toString
            case ">" => a(in).toString > b(in).toString

            case "contains" => a(in).toString contains b(in).toString
            case "containsNot" => !(a(in).toString contains b(in).toString) // todo deprecate
            case "not" if b.toString == "contains" => !(a(in).toString contains b(in).toString)

            // THESE CANNOT CHANGE...

            case "is" if b.toString == "null" => ap.ttype == WTypes.UNDEFINED
            case "not" if b.toString == "null" => ap.ttype != WTypes.UNDEFINED
            case "is" if b.toString == "defined" => ap.ttype != WTypes.UNDEFINED
            case "not" if b.toString == "defined" => ap.ttype == WTypes.UNDEFINED //as.length <= 0

            case "is" if b.toString == "nzlen" => ap.ttype != WTypes.UNDEFINED && as.length > 0 && as.trim != "null"
            case "not" if b.toString == "nzlen" => ap.ttype == WTypes.UNDEFINED || as.length <= 0 || as.trim == "null"

            case "is" if b.toString == "empty" =>
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

            case "is" if b.toString == "undefined" => ap.ttype == WTypes.UNDEFINED
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

              if (al.size != bl.size) {
                false
              } else {
                al.zip(bl).foldLeft(true)((a, b) => a && (b._1 == b._2))
              }
            }

            case "is" => { // is nuber or is date or is string etc
              /* x is TYPE */
              if (b.isInstanceOf[AExprIdent])
                (
                    WTypes.isSubtypeOf(a.getType, b.asInstanceOf[AExprIdent].expr) ||
                    WTypes.isSubtypeOf(ap.calculatedTypedValue.contentType, b.asInstanceOf[AExprIdent].expr) ||
                    WTypes.isSubtypeOf(as, bp.calculatedValue)
                )
              else
              /* if type expr not known, then behave like equals */
                (as == b(in).toString)
            }


            // also should be
            // todo why also should be ???
            case "not" => a(in).toString.length > 0 && a(in) != b(in)

            case "==" => a(in) == b(in)

            case _ if op.trim == "" => {
              // no op - look for boolean parms?
              ap.ttype == WTypes.BOOLEAN && "true" == a(in).toString
            }

            case _ => {
              clog << s"[ERR (a) Operator $op UNKNOWN!!!] as in $a $op $b";
              false
            }
          }
        }
      }
    } catch {
      case t @ _ => throw new DieselExprException("Can't typecast to: " + t.toString).initCause(t)
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
      case "~="  => ai == bi
      case "!="  => ai != bi
      case "<="  => ai <= bi
      case ">="  => ai >= bi
      case "<"   => ai < bi
      case ">"   => ai > bi
      case "is"  => ai == bi
      case "not" => ai != bi
      case _ => {
        clog << s"[ERR Operator $op UNKNOWN!!!] as in $ai $op $bi" ;
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
