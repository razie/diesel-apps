/*   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.expr

import razie.diesel.dom.RDOM.P
import razie.diesel.dom._
import razie.diesel.engine.exec.EEFunc
import razie.diesel.engine.nodes.EMsg
import razie.diesel.engine.{AstKinds, DieselAppContext, DomAst}

/** a "function-like" call:
  * - built-in functions,
  * - msg functions (exec'd in same engine, sync)
  * - domain functions / class members
  */
case class AExprFunc(val expr: String, parms: List[RDOM.P]) extends Expr {

  override def apply(v: Any)(implicit ctx: ECtx) = applyTyped(v).calculatedValue
  override def applyTyped(v: Any)(implicit ctx: ECtx): P = {

    // calc first parm value
    def firstParm = {
      parms.headOption
          .flatMap { p =>
            // maybe the first parm is the accessor expression (lambda parm like)
            val pv = if (p.name.contains(".") || p.name.contains("[")) {
              (new SimpleExprParser).parseIdent(p.name).flatMap(_.tryApplyTyped(v))
            } else if(p.dflt.isEmpty && p.expr.isEmpty) {
              // sizeOf(payload)
              ctx.getp(p.name) // don't care about names, just get the first parm and evalueate
            } else {
              // nope - it's just a normal parm=expr
              Some(p)
            }
            pv
          }.map (_.calculatedP)
      }

    // is it built-in or generic?
      expr match {

        case "sizeOf" => {
          firstParm.map { p =>
            val pv = p.calculatedTypedValue

            if (pv.contentType == WTypes.ARRAY) {
              val sz = pv.asArray.size
              P.fromTypedValue("", sz, WTypes.wt.NUMBER)
            } else if (pv.contentType == WTypes.JSON) {
              val sz = pv.asJson.size
              P.fromTypedValue("", sz, WTypes.wt.NUMBER)
            } else {
              throw new DieselExprException(
                "Not array: " + p.name + " is:" + pv.toString
              )
            }
          }
              .getOrElse(
                throw new DieselExprException(s"No arguments for $expr")
              )
        }

        case "typeOf" => {
            firstParm.map { p =>
              val pv = p.calculatedTypedValue
              P("", pv.contentType, WTypes.wt.STRING).withValue(pv.contentType, WTypes.wt.STRING)
            }.getOrElse(
              throw new DieselExprException(s"No arguments for $expr")
            )
        }

        case "flatten" => {
          firstParm.map { p =>
            val av = p.calculatedP
            p.calculatedTypedValue.cType.name match {
              case WTypes.ARRAY => {
                val elementType = av.calculatedTypedValue.cType.subType

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
              if(ctx.isInstanceOf[StaticECtx])
                ctx.asInstanceOf[StaticECtx].curNode.flatMap(n=>
                  ctx.root.engine.map(_.findLevel(n))
                ).getOrElse(0)
              else
                0

            // a message with this name found, call it sync
            val res = newe.execSync(ast, level, ctx)

            ast.setKinds(AstKinds.TRACE)
            ast.kind = AstKinds.SUBTRACE

            // save the trace in the main tree
            if(ctx.isInstanceOf[StaticECtx])
              ctx.asInstanceOf[StaticECtx].curNode.foreach(_.childrenCol.append(
                ast
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


