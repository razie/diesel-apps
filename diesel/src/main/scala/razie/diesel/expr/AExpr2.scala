/**   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.expr

import org.json.JSONObject
import razie.diesel.dom.RDOM.{P, PValue}
import razie.diesel.dom._
import scala.collection.mutable
import scala.util.parsing.json.JSONArray

/** arithmetic expressions on various types, including json, strings, arrays etc */
case class AExpr2(a: Expr, op: String, b: Expr) extends Expr {
  val expr = (a.toDsl + op + b.toDsl)

  override def apply(v: Any)(implicit ctx: ECtx) = applyTyped(v).currentValue.ee

  /** apply this function to an input value and a context */
  override def applyTyped(v: Any)(implicit ctx: ECtx): P = { //Try {

    // resolve an expression to P with value and type
    def top(x: Expr): Option[P] = x match {
      case CExpr(aa, tt)   => Some(P.fromTypedValue("", aa, tt))
      case aei:AExprIdent  => aei.tryApplyTyped(v)
      case _               => Some(P("", P.asString(a(v))))
    }

    def isNum(x: Expr): Boolean = x match {
      case CExpr(_, WTypes.wt.NUMBER) => true
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
              PValue(ai * bi, WTypes.wt.NUMBER)
            } else {
              val ai = as.toInt
              val bi = b(v).toString.toInt
              PValue(ai * bi, WTypes.wt.NUMBER)
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
            PValue(jsonExpr(op, a(v).toString, b(v).toString), WTypes.wt.JSON)

          // json exprs are different, like cart + { item:...}
          case (aei:AExprIdent, bei:AExprIdent)
            if aei.tryApplyTyped("").exists(_.ttype == WTypes.JSON) &&
                bei.tryApplyTyped("").exists(_.ttype == WTypes.JSON) =>
            PValue(jsonExpr(op, a(v).toString, b(v).toString), WTypes.wt.JSON)

          case _ if isNum(a) && isNum (b) => {
            // if a is num, b will be converted to num
            val as = a(v).toString
            if (as.contains(".")) {
              val ai = as.toFloat
              val bi = b(v).toString.toFloat
              PValue(ai + bi, WTypes.wt.NUMBER)
            } else {
              val ai = as.toInt
              val bi = b(v).toString.toInt
              PValue(ai + bi, WTypes.wt.NUMBER)
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
              PValue(res, WTypes.wt.ARRAY)
           } else  if(bv.ttype == WTypes.JSON ||
                  av.ttype == WTypes.JSON) {
              // json exprs are different, like cart + { item:...}
              PValue(jsonExpr(op, a(v).toString, b(v).toString), WTypes.wt.JSON)
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
              PValue(ai - bi, WTypes.wt.NUMBER)
            } else {
              val ai = as.toInt
              val bi = b(v).toString.toInt
              PValue(ai - bi, WTypes.wt.NUMBER)
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

      case "as" => {
        val av = a.applyTyped(v).calculatedTypedValue
        b match {
          case _ if b.toString == "number" =>
            P.fromTypedValue("", av.value, WTypes.NUMBER).calculatedTypedValue

          case _ if b.toString == "string" =>
            P.fromTypedValue("", av.value, WTypes.STRING).calculatedTypedValue

          case t : CExpr[String] => // x as "application/pdf"
            P("", av.value.toString, WType(t.ee)).withValue(av.value, t.ee).calculatedTypedValue

          case _ => throw new DieselExprException("Can't typecast to: " + b.toString)
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


