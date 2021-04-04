/*   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.expr

import razie.diesel.dom.RDOM.P
import razie.diesel.dom._
import razie.diesel.engine.nodes.CanHtml

/** marker exception class for expr - understood by AST engine and treated nicer than a random exception */
class DieselExprException (msg:String) extends RuntimeException (msg)
// to add a base, use withcause or smth

/** element has DSL form
  *
  *  deserialization is assumed via parser
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
  * Basic executable/actionable interface. These process a default input value and return a Typed output value.
  *
  * They are also invoked in a context - a set of objects in a certain role.
  *
  * There are two major branches: WFunc and WfActivity. An action is a workflow specific thing and is aware of next actions, state of execution whatnot. It also does something so it's derived from WFunc.
  *
  * WFunc by itself only does something and is not stateful. Most activities are like that.
  */
trait WFunc {

  /** apply this function to an input value and a context */
  def apply(v: Any)(implicit ctx: ECtx): Any

  /** apply this function to an input value and a context */
  def applyTyped(v: Any)(implicit ctx: ECtx): P = {
    val res = apply(v)
    // todo use the PValue
    P.fromTypedValue("", res, getType)
  }

  /** what is the resulting type - when known */
  def getType: WType = WTypes.wt.EMPTY
}

/** an expression */
abstract class Expr extends WFunc with HasDsl with CanHtml {
  /** the string form - why do I have this on top of serialize and toDsl ? */
  def expr: String

  override def toString = toDsl
  override def toDsl = expr
  override def toHtml = tokenValue(toDsl)
}

/** a block i.e. ( expr ) */
case class BlockExpr(ex: Expr) extends Expr {
  val expr = "( " + ex.toString + " )"

  override def apply(v: Any)(implicit ctx: ECtx) = ex.apply(v)
  override def applyTyped(v: Any)(implicit ctx: ECtx): P = ex.applyTyped(v)
  override def getType: WType = ex.getType
}

