/*   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.expr

import com.mongodb.casbah.Imports.ObjectId
import java.net.URLEncoder
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.TimeZone
import razie.diesel.Diesel
import razie.diesel.dom.RDOM.P
import razie.diesel.dom._
import razie.diesel.engine.exec.EEFunc
import razie.diesel.engine.nodes.EMsg
import razie.diesel.engine.{AstKinds, DieselAppContext, DomAst}
import razie.wiki.{Enc, EncUrl}

/** a "function-like" call:
  * - built-in functions,
  * - msg functions (exec'd in same engine, sync)
  * - domain functions / class members
  */
case class AExprFunc(val expr: String, parms: List[RDOM.P]) extends Expr {

  override def apply(v: Any)(implicit ctx: ECtx) = applyTyped(v).calculatedValue
  override def applyTyped(v: Any)(implicit ctx: ECtx): P = {

    // calc first parm value
    def aParm(op: Option[P]) = {
      op
          .flatMap { p =>
            // maybe the first parm is the accessor expression (lambda parm like)
            val pv = if (p.dflt.isEmpty && p.expr.isEmpty) {
              if (p.name.contains(".") || p.name.contains("["))
                (new SimpleExprParser).parseIdent(p.name).flatMap(_.tryApplyTyped(v))
              else
                ctx.getp(p.name) // don't care about names, just get the first parm and evalueate
            } else {
              // nope - it's just a normal parm=expr
              Some(p)
            }
            pv
          }.map(_.calculatedP)
    }

    // calc first parm value
    def firstParm = aParm(parms.headOption)

    // calc first parm value
    def secondParm = aParm(parms.drop(1).headOption)

    // is it built-in or generic?
    expr match {

      case "uuid" => {
        // todo singleton
        val x = new ObjectId().toString

        P.fromTypedValue("", x, WTypes.wt.STRING)
      }

      case "now" => {
        // todo singleton
        val tsFmtr = DateTimeFormatter.ofPattern(WTypes.DATE_FORMAT)
        val nw = LocalDateTime.now
        val ts = tsFmtr.format(nw)

        P.fromTypedValue("", ts, WTypes.wt.DATE)
      }

      case "sizeOf" => {
        firstParm.map { p =>
          val pv = p.calculatedTypedValue

          if (pv.contentType == WTypes.ARRAY) {
            val sz = pv.asArray.size
            P.fromTypedValue("", sz, WTypes.wt.NUMBER)
          } else if (pv.contentType == WTypes.JSON) {
            val sz = pv.asJson.size
            P.fromTypedValue("", sz, WTypes.wt.NUMBER)
          } else if (pv.contentType == WTypes.STRING) {
            val sz = pv.asString.length
            P.fromTypedValue("", sz, WTypes.wt.NUMBER)
          } else {
            throw new DieselExprException(
              "Not array: " + p.name + " is:" + pv.toString
            )
          }
        }
            .getOrElse(
              // more failure resistant
              P.fromTypedValue("", 0, WTypes.wt.NUMBER)
            )
      }

      case "typeOf" => {
        firstParm.map { p =>
          val pv = p.calculatedTypedValue
          P("", pv.contentType, WTypes.wt.STRING).withValue(pv.contentType, WTypes.wt.STRING)
        }.getOrElse(
          // todo could be unknown?
          throw new DieselExprException(s"No arguments for $expr")
        )
      }

      case "urlencode" => {
        firstParm.map { p =>
          val p1 = p.calculatedValue
          val pv = URLEncoder.encode(p1, "UTF8")
          P("", pv, WTypes.wt.STRING).withValue(pv, WTypes.wt.STRING)
        }.getOrElse(
          throw new DieselExprException(s"No arguments for $expr")
        )
      }

      case "accessor" => {
        firstParm.map { p =>
          val pStart = p.calculatedP


          // parse second parm as aexprident
          secondParm
              .orElse {
                throw new DieselExprException(s"No second argument for $expr")
              }
              .flatMap { p =>
                val pv = if (p.dflt.isEmpty && p.expr.isEmpty) {
                  P("", p.name)
                } else {
                  // nope - it's just a normal parm=expr
                  p.calculatedP // need to do this to not affect the original with cached value
                }

                val p1 = pv.calculatedValue
                val pa =
                  (new SimpleExprParser).parseIdent(p1).flatMap(_.tryApplyTypedFrom(Some(pStart)))
                pa
              }.getOrElse(
            P.undefined(Diesel.PAYLOAD)
          )
        }.getOrElse(
          throw new DieselExprException(s"No first argument for $expr")
        )
      }

      case "flatten" => {

        // the flat part of flatmap

        firstParm.map { p =>
          val av = p.calculatedP
          p.calculatedTypedValue.cType.name match {
            case WTypes.ARRAY => {
              val elementType = av.calculatedTypedValue.cType.wrappedType

              val arr = av.calculatedTypedValue.asArray.asInstanceOf[List[List[_]]]
              val resArr = arr.flatMap { x =>
                if (x.isInstanceOf[List[Any]])
                  x.asInstanceOf[List[Any]]
                else
                  throw new DieselExprException("Can't flatten element: " + x)
              }

              val finalArr = resArr
              P.fromTypedValue("", finalArr, WTypes.wt.ARRAY)
            }

              case _ => throw new DieselExprException("Can't do flatten on: " + av)
            }
          }
            }.getOrElse(
          throw new DieselExprException(s"No arguments for $expr")
        )

        case _ => {

        // must be in form x...y.func
        val PAT = """([\w.]+)[./](\w+)""".r
        val PAT(ee, aa) = expr
        val msg = EMsg(ee, aa, parms)
        val ast = DomAst(msg, AstKinds.RECEIVED)

        // is there a spec for it in current domain?
        val spec = ctx.root.domain.flatMap {
          _.moreElements.collect {
            case s: EMsg if s.ea == expr => Some(s)
          }.headOption
        }

        // or is it defined as a func in domain?
        val func = ctx.root.domain.flatMap {
          _.funcs.get (expr)
        }

        spec.flatMap { msgSpec =>
          ctx.root.engine.flatMap{engine=>
            val newe = DieselAppContext.mkEngine(
              engine.dom,
              ast,
              engine.settings,
              engine.pages,
              "SYNC-"+engine.description
            )

            val level =
              if (ctx.isInstanceOf[SimpleECtx])
                ctx.asInstanceOf[SimpleECtx].curNode.flatMap(n =>
                  ctx.root.engine.map(_.findLevel(n))
                ).getOrElse(0)
              else
                0

            // a message with this name found, call it sync
            val res = newe.execSync(ast, level, ctx)

            ast.setKinds(AstKinds.TRACE)
            ast.kind = AstKinds.SUBTRACE

            // save the trace in the main tree
            if (ctx.isInstanceOf[StaticECtx])
              ctx.asInstanceOf[StaticECtx].curNode.foreach(_.appendAllNoEvents(
                List(ast)
              ))

            res
          }
        } orElse func.map {f=>
            // todo add more ast info?
          EEFunc.exec(msg, f)
        } getOrElse {
          throw new DieselExprException("Function/Message not found: " + expr)
        }
      }
    }

  }

  override def toDsl = expr + "(" + parms.mkString(",") + ")"
  override def toHtml = tokenValue(toDsl)
}


