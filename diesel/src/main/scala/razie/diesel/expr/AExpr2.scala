/*   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.expr

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import org.json.JSONObject
import razie.diesel.dom.RDOM.{P, PValue}
import razie.diesel.dom._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try
import scala.util.parsing.json.JSONArray

/** type-aware arithmetic expressions of the form "a OP b" - on various types, including json, strings, arrays etc */
case class AExpr2(a: Expr, op: String, b: Expr) extends Expr {
  val expr = "("+a.toDsl + " " + op + " " + b.toDsl+")"

  override def apply(v: Any)(implicit ctx: ECtx) = applyTyped(v).currentValue.ee

  /** apply this function to an input value and a context */
  override def applyTyped(v: Any)(implicit ctx: ECtx): P = { //Try {

    // resolve an expression to P with value and type
    def top(x: Expr): Option[P] = x match {
      case CExpr(aa, tt) => Some(P.fromTypedValue("", aa, tt))
      case aei: AExprIdent => aei.tryApplyTyped(v)
      case _ => Some(P("", P.asString(a(v))))
    }

    def isNum(p: P): Boolean = p.calculatedTypedValue.cType.name == WTypes.NUMBER

    def isDate(p: P): Boolean = p.calculatedTypedValue.cType.name == WTypes.DATE

    val av = a.applyTyped(v)

    val res: PValue[_] = op match {
      case "/" => {
        val bv = b.applyTyped(v)
        (a, b) match {
          case _ if isNum(av) && isNum(bv) => {
            val as = av.calculatedValue
            if (as.contains(".")) {
              val ai = as.toFloat
              val bi = bv.calculatedValue.toFloat
              PValue(ai / bi, WTypes.wt.NUMBER)
            } else {
              val ai = as.toLong
              val bi = bv.calculatedValue.toLong
              PValue(ai / bi, WTypes.wt.NUMBER)
            }
            // todo float and type safe numb
          }

          case _ => {
            PValue("ERR:operator / on non numbers")
            throw new DieselExprException(
              s"ERR: multiply needs both numbers but got: ${a.getType} and ${b.getType} "
            )
          }
        }
      }

      case "*" => {
        val bv = b.applyTyped(v)
        (a, b) match {
          case _ if isNum(av) && isNum(bv) => {
            val as = av.calculatedValue
            if (as.contains(".")) {
              val ai = as.toFloat
              val bi = bv.calculatedValue.toFloat
              PValue(ai * bi, WTypes.wt.NUMBER)
            } else {
              val ai = as.toLong
              val bi = bv.calculatedValue.toLong
              PValue(ai * bi, WTypes.wt.NUMBER)
            }
            // todo float and type safe numb
          }

          case _ => {
            PValue("ERR:operator * on non numbers")
            throw new DieselExprException(
              s"ERR: multiply needs both numbers but got: ${a.getType} and ${b.getType} "
            )
          }
        }
      }

      case "+" => {
        val bv = b.applyTyped(v)
        (a, b) match {
          // json exprs are different, like cart + { item:...}
          case (aei:AExprIdent, JBlockExpr(jb, _))
            if aei.tryApplyTyped("").exists(_.ttype == WTypes.JSON) =>
            jsonExpr(op, a(v).toString, b(v).toString)

          // json exprs are different, like cart + { item:...}
          case (aei:AExprIdent, bei:AExprIdent)
            if aei.tryApplyTyped("").exists(_.ttype == WTypes.JSON) &&
                bei.tryApplyTyped("").exists(_.ttype == WTypes.JSON) =>
            jsonExpr(op, a(v).toString, b(v).toString)

          case _ if isDate(av) => {
            val as = a(v).toString
            val bs = b(v).toString
            if (!bs.matches("[0-9]+ *[nsmhdMy]"))
              throw new DieselExprException("Right side not a duration like 5s : " + bs)
            val bi = bs.substring(0, bs.length - 1).trim.toLong
            val bt = bs.last

            val tsFmtr = DateTimeFormatter.ofPattern(WTypes.DATE_FORMAT)
            val ad = LocalDateTime.from(tsFmtr.parse(as))

            var res = ad

            bt match {
              case 'n' => res = res.plusNanos(bi)
              case 's' => res = res.plusSeconds(bi)
              case 'm' => res = res.plusMinutes(bi)
              case 'h' => res = res.plusHours(bi)
              case 'd' => res = res.plusDays(bi)
              case 'M' => res = res.plusMonths(bi)
              case 'y' => res = res.plusYears(bi)
              case _ =>
                throw new DieselExprException("Unknown duration type [nsmhdMy] : " + bt)
            }

            val ts = tsFmtr.format(res)
            PValue(ts, WTypes.wt.DATE).withStringCache(ts)
          }

          case _ if isNum(av) && isNum(bv) => {
            // if a is num, b will be converted to num
            val as = a(v).toString
            if (as.contains(".")) {
              val ai = as.toFloat
              val bi = b(v).toString.toFloat
              PValue(ai + bi, WTypes.wt.NUMBER)
            } else {
              val ai = as.toLong
              val bi = b(v).toString.toLong
              PValue(ai + bi, WTypes.wt.NUMBER)
            }
          }

          case _ => {
            // concat lists
            if (bv.ttype == WTypes.ARRAY ||
                av.ttype == WTypes.ARRAY) {
              val al = if (av.ttype == WTypes.ARRAY) av.calculatedTypedValue.asArray else List(
                av.calculatedTypedValue.value)
              val bl = if (bv.ttype == WTypes.ARRAY) bv.calculatedTypedValue.asArray else List(
                bv.calculatedTypedValue.value)
              val res = new ListBuffer[Any]()
              res.appendAll(al)
              res.appendAll(bl)
              PValue(res, WTypes.wt.ARRAY)
            } else if (bv.ttype == WTypes.JSON ||
                av.ttype == WTypes.JSON) {
              // json exprs are different, like cart + { item:...}
              try {
                jsonExpr(op, a(v).toString, b(v).toString)
              } catch {
                case t: Throwable => throw new DieselExprException(
                  s"Parm ${av} or ${bv} can't be parse to JSON: " + t.toString).initCause(t)
              }
            } else {
              PValue(av.calculatedValue + bv.calculatedValue)
            }
          }
        }
      }

      case "-" => {
        val bv = b.applyTyped(v)
        (a, b) match {
          // json exprs are different, like cart + { item:...}
          case (aei: AExprIdent, JBlockExpr(jb, _))
            if aei.tryApplyTyped("").exists(_.ttype == WTypes.JSON) =>
            jsonExpr(op, a(v).toString, b(v).toString)

          // json exprs are different, like cart + { item:...}
          case (aei: AExprIdent, bei: AExprIdent)
            if aei.tryApplyTyped("").exists(_.ttype == WTypes.JSON) &&
                bei.tryApplyTyped("").exists(_.ttype == WTypes.JSON) =>
            jsonExpr(op, a(v).toString, b(v).toString)

          case _ if isNum(av) && isNum(bv) => {
            // if a is num, b will be converted to num
            val as = av.calculatedValue
            if (as.contains(".")) {
              val ai = as.toFloat
              val bi = bv.calculatedValue.toFloat
              PValue(ai - bi, WTypes.wt.NUMBER)
            } else {
              val ai = as.toLong
              val bi = bv.calculatedValue.toLong
              PValue(ai - bi, WTypes.wt.NUMBER)
            }
          }

          case _ => {
            throw new DieselExprException(
              "[ERR can't apply operator " + op + s" to types ${av.ttype} and ${bv.ttype}] (" + av + " , " + bv + ")"
            )
          }
        }
      }

      // like in JS, if first not exist, use second
      case "||" if a.isInstanceOf[AExprIdent] => {
        a match {
          case aei:AExprIdent =>
            aei.tryApplyTyped("")
                .filter(x => !x.isUndefinedOrEmpty)
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

        def doAsWith(b: Expr, bu: String, bs: String, lastTime: Boolean): PValue[_] = {
          val ap = av.calculatedP
          val avv = av.calculatedTypedValue
          val as = avv.asString

          b match {

            // known types

            case _ if bs == "boolean" || bs == "bool" =>
              P.fromTypedValue("", as, WTypes.BOOLEAN).calculatedTypedValue

            case _ if bs == "number" =>
              P.fromTypedValue("", as, WTypes.NUMBER).calculatedTypedValue

            case _ if bs == "string" =>
              P.fromTypedValue("", as, WTypes.STRING).calculatedTypedValue

            case _ if bs == "json" || bs == "object" =>
              P.fromTypedValue("", as, WTypes.JSON).calculatedTypedValue

            case _ if bs == "date" =>
              P.fromTypedValue("", as, WTypes.DATE).calculatedTypedValue

            case _ if bs == "array" => {
              if (avv.cType.name == WTypes.JSON || avv.cType.name == WTypes.OBJECT) {
                // map to array
                val arr = avv.asJson

                val resArr = arr.map { x =>
                  val xj = P.fromSmartTypedValue("x", Map("key" -> x._1, "value" -> x._2))
                  xj
                }
                P.fromTypedValue("", resArr.toList, WTypes.ARRAY).calculatedTypedValue
              } else {
                // maybe string to array
                P.fromTypedValue("", as, WTypes.ARRAY).calculatedTypedValue
              }
            }

            case c if ctx.root.domain.exists(_.classes.contains(bu)) => {
              // domain class, base must be json
              assert(ap.isOfType(WTypes.wt.JSON))
              avv.copy(cType = avv.cType.copy(schema = bu))
            }

            case t: CExpr[String] => // x as "application/pdf"
              // todo smarter, like "asdf" as Student etc - implicit constructors, typecasts etc
              if (av.value.isDefined)
              // todo should we only change theP type not the PVAlue type? like pdf which is a bitstream?
                P("", av.calculatedValue, WType(t.ee)).withValue(avv.value, WType(t.ee)).calculatedTypedValue
              else
                P("", as, WType(t.ee)).calculatedTypedValue

            case _ if !lastTime => {
              val bp = b.applyTyped(v)
              doAsWith(bp.currentValue, bp.currentStringValue, b.toString.toLowerCase, true)
            }

            case _ if lastTime => throw new DieselExprException("Can't typecast to: " + b.toString + " from: " + av)
          }
        }

        doAsWith(b, b.toString, b.toString.toLowerCase, false)
      }

      case "map" => {
        av.calculatedTypedValue.cType.name match {
          case WTypes.ARRAY => {
            val elementType = av.calculatedTypedValue.cType.wrappedType

            val arr = av.calculatedTypedValue.asArray

            val resArr = arr.map { x =>
              val res = if (b.isInstanceOf[LambdaFuncExpr]) {
                val res = b.applyTyped(x)
                res
              } else if (b.isInstanceOf[BlockExpr] && b.asInstanceOf[BlockExpr].ex.isInstanceOf[LambdaFuncExpr]) {
                // common case, no need to go through context, Block passes through to Lambda
                val res = b.applyTyped(x)
                res
              } else {
                // we populate an "x" or should it be "elem" ?
                val sctx = new StaticECtx(List(P.fromTypedValue("x", x)), Some(ctx))
                val res = b.applyTyped(x)(sctx)
                res
              }
              res
            }

            val finalArr = resArr.map(_.calculatedTypedValue.value)
            PValue(finalArr, WTypes.wt.ARRAY)
          }

          case WTypes.JSON => {
            val elementType = av.calculatedTypedValue.cType.getClassName

            val arr = av.calculatedTypedValue.asJson

            val resArr = arr.map { x =>
              val xj = P.fromSmartTypedValue("x", Map("key" -> x._1, "value" -> x._2))

              val res = if (b.isInstanceOf[LambdaFuncExpr]) {
                // arr map x => f
                val res = b.applyTyped(xj)
                res
              } else if (b.isInstanceOf[BlockExpr] && b.asInstanceOf[BlockExpr].ex.isInstanceOf[LambdaFuncExpr]) {
                // arr map (x => f)
                // common case, no need to go through context, Block passes through to Lambda
                val res = b.applyTyped(xj)
                res
              } else {
                // we populate an "x" or should it be "elem" ?
                val sctx = new StaticECtx(List(P.fromTypedValue("x", x)), Some(ctx))
                val res = b.applyTyped(xj)(sctx)
                res
              }
              res
            }

            val finalArr = resArr.map(_.calculatedTypedValue.value)
            PValue(finalArr, WTypes.wt.ARRAY)
          }

          case _ => throw new DieselExprException("Can't do map on: " + av)
        }
      }

      case "mkString" => {

        av.calculatedTypedValue.cType.name match {
          case WTypes.ARRAY => {
            val elementType = av.calculatedTypedValue.cType.wrappedType

            val arr = av.calculatedTypedValue.asArray

            val resArr = arr.map { x =>
              if (x.isInstanceOf[P]) {
                x.asInstanceOf[P].currentStringValue
              } else if (x.isInstanceOf[Expr]) {
                x.asInstanceOf[Expr].applyTyped(v).currentStringValue
              } else if (x.isInstanceOf[PValue[_]]) {
                x.asInstanceOf[PValue[_]].asString
              } else {
                x.toString
              }
            }

            val res = resArr.mkString(b.applyTyped("").currentStringValue)

            PValue(res, WTypes.wt.STRING)
          }

          case _ => throw new DieselExprException("Can't do mkString on: " + av)
        }
      }

      case "flatMap" => {
        av.calculatedTypedValue.cType.name match {
          case WTypes.ARRAY => {
            val elementType = av.calculatedTypedValue.cType.wrappedType

            val arr = av.calculatedTypedValue.asArray

            val resArr = arr.flatMap {x=>
              val res = if(b.isInstanceOf[LambdaFuncExpr]) {
                val resp = b.applyTyped(x).calculatedTypedValue
                if(!resp.cType.equals(WTypes.wt.ARRAY)) throw new DieselExprException("Result of right side not Array!")
                val res = resp.asArray
                res
              } else if(b.isInstanceOf[BlockExpr] && b.asInstanceOf[BlockExpr].ex.isInstanceOf[LambdaFuncExpr]) {
                // common case, no need to go through context, Block passes through to Lambda
                val resp = b.applyTyped(x).calculatedTypedValue
                if(!resp.cType.equals(WTypes.wt.ARRAY)) throw new DieselExprException("Result of right side not Array!")
                val res = resp.asArray
                res
              } else {
                // we populate an "x" or should it be "elem" ?
                val sctx = new StaticECtx(List(P.fromTypedValue("x", x)), Some(ctx))
                val resp = b.applyTyped(x)(sctx).calculatedTypedValue
                if(!resp.cType.equals(WTypes.wt.ARRAY)) throw new DieselExprException("Result of right side not Array!")
                val res = resp.asArray
                res
              }
              res
            }

            val finalArr = resArr
            PValue(finalArr, WTypes.wt.ARRAY)
          }

          case _ => throw new DieselExprException("Can't do flatMap on: " + av)
        }
      }

      case "filter" => {
        val av = a.applyTyped(v).calculatedTypedValue

        av.cType.name match {
          case WTypes.ARRAY => {
            val elementType = av.cType.wrappedType

            val arr = av.asArray

            val resArr = arr.filter {x=>
              val res = if(b.isInstanceOf[LambdaFuncExpr]) {
                val res = b.applyTyped(x)
                res
              } else {
                // we populate an "x" or should it be "elem" ?
                val sctx = new StaticECtx(List(P.fromTypedValue("x", x)), Some(ctx))
                val res = b.applyTyped(x)(sctx)
                res
              }
              Try {
                res.calculatedTypedValue.asBoolean
              }.getOrElse {
                throw new DieselExprException("Not a boolean expression: " + res.calculatedTypedValue)
              }
            }

            val finalArr = resArr
            PValue(finalArr, WTypes.wt.ARRAY)
          }

          case _ => PValue(new DieselExprException("Can't do filter on: " + av), WTypes.wt.EXCEPTION)
        }
      }

      case "exists" => {
        val av = a.applyTyped(v).calculatedTypedValue

        av.cType.name match {
          case WTypes.ARRAY => {
            val elementType = av.cType.wrappedType

            val arr = av.asArray

            val resArr = arr.exists {x=>
                val res = if(b.isInstanceOf[LambdaFuncExpr]) {
                  val res = b.applyTyped(x)
                  res
                } else {
                  // we populate an "x" or should it be "elem" ?
                  val sctx = new StaticECtx(List(P.fromTypedValue("x", x)), Some(ctx))
                  val res = b.applyTyped(x)(sctx)
                  res
                }
              Try {
                res.calculatedTypedValue.asBoolean
              }.getOrElse {
                throw new DieselExprException("Not a boolean expression: " + res.calculatedTypedValue)
              }
            }

            PValue(resArr, WTypes.wt.BOOLEAN)
          }

//          case _ => throw new DieselExprException("Can't do exists on: " + av)
          case _ => PValue(new DieselExprException("Can't do exists on: " + av), WTypes.wt.EXCEPTION)
        }
      }

      case _ => PValue("[ERR unknown operator " + op + "]", WTypes.wt.ERROR)
    }

    P("", res.asString, res.cType).copy(value = Some(res))
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

  def jsonExpr(op: String, aa: String, bb: String) = {
    val ai = try {
      razie.js.parse(aa)
    } catch {
      case t:Throwable => throw new DieselExprException(s"Parm ${aa} can't be parsed to JSON: " + t.toString).initCause(t)
    }
    val bi = try {
      razie.js.parse(bb)
    } catch {
      case t: Throwable => throw new DieselExprException(
        s"Parm ${bb} can't be parsed to JSON: " + t.toString).initCause(t)
    }
    val res = new mutable.HashMap[String, Any]()

    ai.foreach { t =>
      res.put(t._1, t._2)
    }

    if ("+" == op.trim) {
      bi.foreach { t =>
        val k = t._1
        val bv = t._2
        if (res.contains(k)) {
          val ax = res(k)
          ax match {

            case al: collection.Seq[_] => {
              bv match {
                // add lists
                case bll: collection.Seq[_] => {
                  val l = new ListBuffer[Any]
                  l.appendAll(al)
                  l.appendAll(bll)
                  res.put(k, l)
                }
                case _ => {
                  val l = new ListBuffer[Any]
                  l.appendAll(al)
                  l.append(bv)
                  res.put(k, l)
                }
              }
            }

            case m: collection.Map[_, _] => {
              // merge maps
              val mres = new mutable.HashMap[String, Any]()
              m.foreach { t =>
                mres.put(t._1.toString, t._2)
              }
              res.put(k, mres)
            }

            case y@_ => {
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
    } else if ("-" == op.trim) {
      bi.foreach { t =>
        val k = t._1
        res.remove(k)
      }
    }

    val s = razie.js.tojsons(res.toMap)
    PValue(res, WTypes.wt.JSON).withStringCache(s)
  }

  override def getType = a.getType
}


