package razie.diesel.dom

//------------ expressions and conditions

/** deserialization is assumed via DSL
  *
  *  the idea is that all activities would have an external DSL form as well
  *  and can serialize themselves in that form
  *
  *  serialize the DEFINITION only - not including states/values
  */
trait HasDsl /*extends GReferenceable*/ {
  def serialize : String = toDsl

  /** serialize the DEFINITION - not including
    */
  def toDsl : String
  def toIndentedDsl (indent:Int=>String, level:Int) = indent(level) + toDsl
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
def apply (v:Any)(implicit ctx:ECtx) : Any
}

/** an expression */
abstract class Expr extends WFunc with HasDsl {
  def expr : String
  override def toString = toDsl
  override def toDsl = expr
}



/** arithmetic expressions */
case class AExpr2 (a:Expr, op:String, b:Expr) extends Expr {
  val expr = (a.toDsl+op+b.toDsl)
  override def apply (v:Any)(implicit ctx:ECtx) = op match {
    case "+" => a(v).toString + b(v).toString
    case _ => "[ERR unknown operator "+op+"]"
  }
}

/** arithmetic expressions */
class AExprIdent (val expr:String) extends Expr {
  override def apply (v:Any)(implicit ctx:ECtx) = ctx.apply(expr)
}

/** constant expression
  *
  *  TODO i'm loosing the type definition
  */
case class CExpr (ee : String, ttype:String) extends Expr {
  val expr = ee.toString
  override def apply (v:Any)(implicit ctx:ECtx) = ee
  override def toDsl = if(ttype == "String") ("\"" + expr + "\"") else expr
}


/** arithmetic expressions */
case class AExpr (val expr : String) extends Expr {
  override def apply (v:Any)(implicit ctx:ECtx) = v
}
