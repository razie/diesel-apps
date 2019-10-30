/*   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.expr

import razie.diesel.dom.RDOM.P
import razie.diesel.dom._

/** a range like 1..4 or ..5 etc */
case class ExprRange(val start: Expr, val end: Option[Expr]) extends Expr {
  /** what is the resulting type - when known */
  override def getType: WType = WTypes.wt.RANGE
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


