/*   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.expr

import razie.diesel.dom.RDOM.P
import razie.diesel.dom._
import razie.diesel.exec.EEFunc

/** a js expression
  * js:a.b
  * js:{...}
  */
case class JSSExpr(s: String) extends Expr {
  val expr = "js{{ " + s + " }}"

  override def getType: WType = WTypes.wt.UNKNOWN

  override def apply(v: Any)(implicit ctx: ECtx) =
    EEFunc.execute("js", s) //.dflt

  override def applyTyped(v: Any)(implicit ctx: ECtx): P = {
      EEFunc.executeTyped("js", s)
  }
}

/** a scala expression
  * sc:a.b
  * sc:{...}
  */
case class SCExpr(s: String) extends Expr {
  val expr = "sc{{ " + s + " }}"

  override def getType: WType = WTypes.wt.UNKNOWN

  override def apply(v: Any)(implicit ctx: ECtx) =
    EEFunc.execute("scala", s) //.dflt

  override def applyTyped(v: Any)(implicit ctx: ECtx): P =
    EEFunc.executeTyped("scala", s)
}


