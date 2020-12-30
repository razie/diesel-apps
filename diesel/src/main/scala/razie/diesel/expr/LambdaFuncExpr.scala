/*   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.expr

import razie.diesel.dom.RDOM.P
import razie.diesel.dom._

/** a "function" call: built-in functions, msg functions (exec'd in same engine, sync) */
case class LambdaFuncExpr(val argName:String, val ex: Expr, parms: List[RDOM.P]=Nil) extends Expr {
  override def getType: WType = ex.getType

  override def expr = argName + "=>" + ex.toDsl

  override def apply(v: Any)(implicit ctx: ECtx) = applyTyped(v).calculatedValue

  override def applyTyped(v: Any)(implicit ctx: ECtx): P = {
    val sctx = new StaticECtx(List(P.fromTypedValue(argName, v)), Some(ctx))
    val res = ex.applyTyped(v)(sctx)
    res
  }

  override def toDsl = expr + "(" + parms.mkString(",") + ")"
  override def toHtml = tokenValue(toDsl)
}


