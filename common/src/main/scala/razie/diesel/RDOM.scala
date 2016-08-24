package razie.diesel

/**
 * simple, neutral domain model representation: class/object/function
 *
 * These are collected in RDomain
 */
object RDOM {
  // archtetypes
  val ARCH_SPEC = "Spec"
  val ARCH_ROLE = "Role"
  val ARCH_ENTITY = "Entity"
  val ARCH_MI = "MI"

  def smap(s:String) (f:String=>String) =  if(s != null && s.length > 0) f(s) else ""
  def mks(l:List[_], pre:String, sep:String, post:String="", indent:String="") = if(l.size>0) pre + l.map(indent + _).mkString(sep) + post else ""

  class CM // abstract Class Member

  /** represents a Class */
  case class C (name:String, archetype:String, stereotypes:String, base:List[String], typeParam:String, parms:List[P]=Nil, methods:List[F]=Nil, assocs:List[A]=Nil) {
    override def toString = s"""class <b><a href="/wikie/show/Category:$name">$name</a></b> """ +
      smap(typeParam) (" [" + _ + "]") +
      smap(archetype) (" &lt;" + _ + "&gt;") +
      smap(stereotypes) (" &lt;" + _ + "&gt;") +
      (if(base.exists(_.size>0)) "extends " else "") + base.map("<b>" + _ + "</b>").mkString +
      mks(parms, " (", ", ", ") ") +
      mks(methods, "{<br>", "<br>", "<br>}", "&nbsp;&nbsp;")

    def fullHtml = s"""class <b><a href="/wikie/show/Category:$name">$name</a></b> """ +
      smap(typeParam) (" [" + _ + "]") +
      smap(archetype) (" &lt;" + _ + "&gt;") +
      smap(stereotypes) (" &lt;" + _ + "&gt;") +
      (if(base.exists(_.size>0)) "extends " else "") + base.map("<b>" + _ + "</b>").mkString +
      mks(parms, " (<br>", ",<br> ", ") ", "&nbsp;&nbsp;") +
      mks(methods, "{<br><hr>", "<br>", "<br><hr>}", "&nbsp;&nbsp;")
  }

  def quot(s:String) = "\""+ s + "\""

  /** represents a parameter/member/attribute */
  case class P (name:String, ttype:String, ref:String, multi:String, dflt:String, expr:Option[Expr]=None) extends CM {
    def this (name:String, dflt:String) = this(name,"","","",dflt)
    override def toString =
      s"$name" +
        smap(ttype) (":" + ref + _) +
        smap(multi)(identity) +
        smap(dflt) (s=> "=" + quot(s)) +
        (if(dflt=="") expr.map(x=>smap(x.toString) ("=" + _)) else "")

    // todo refs for type, docs, position etc
    def toHtml =
      s"<b>$name</b>" +
        smap(ttype) (":" + ref + _) +
        smap(multi)(identity) +
        smap(dflt) (s=> "=" + quot(s)) +
        (if(dflt=="") expr.map(x=>smap(x.toString) ("=" + _)) else "")
  }

  /** represents a parameter match expression */
  case class PM (name:String, ttype:String, ref:String, multi:String, op:String, dflt:String, expr:String = "") extends CM {
    override def toString =
      s"$name" +
        smap(ttype) (":" + ref + _) +
        smap(multi)(identity) +
        smap(dflt) (s=> op + quot(s)) //+
    //      (if(dflt=="") expr.map(x=>smap(x.toString) ("=" + _)) else "")

    def toHtml =
      s"<b>$name</b>" +
      smap(ttype) (":" + ref + _) +
      smap(multi)(identity) +
      smap(dflt) (s=> op + quot(s)) //+
//      (if(dflt=="") expr.map(x=>smap(x.toString) ("=" + _)) else "")
  }

  /** a function / method */
  case class F (name:String, parms:List[P], ttype:String, script:String="", body:List[EXEC]=List.empty) extends CM {
    override def toString = s"  def <b>$name</b> " +
      mks(parms, " (", ", ", ") ") +
      smap(ttype) (":" + _)
  }

  /** an executable statement */
  trait EXEC {
    def sForm:String
    def exec(ctx:Any, parms:Any*):Any

    override def toString = sForm
  }

  case class V (name:String, value:String)  // attr value

  case class O (name:String, base:String, parms:List[V]) { // object = instance of class
    def toJson = parms.map{p=> p.name -> p.value}.toMap
  }

  /** Diamond */
  class D  (val roles:List[(String, String)], val ac:Option[AC]=None) //diamond association

  /** name is not required */
  case class A  (name:String, a:String, z:String, aRole:String, zRole:String, parms:List[P]=Nil, override val ac:Option[AC]=None) //association
    extends D (List(a->aRole, z->zRole), ac) {
    override def toString = s"assoc $name $a:$aRole -> $z:$zRole " +
      mks(parms, " (", ", ", ") ")
  }
  case class AC (name:String, a:String, z:String, aRole:String, zRole:String, cls:C) //association class

  // Diamond Class

  case class E (name:String, parms:List[P], methods:List[F]) //event
  case class R (name:String, parms:List[P], body:String) //rule
  case class X (body:String) //expression
  case class T (name:String, parms:List[P], body:String) //pattern

  case class TYP (name:String, ref:String, kind:String, multi:String)

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
  abstract class Expr (val expr : String) extends WFunc with HasDsl {
    override def toString = toDsl
    override def toDsl = expr
  }

  /** arithmetic expressions */
  class AExpr (val e : String) extends Expr (e) {
    override def apply (v:Any)(implicit ctx:ECtx) = v
  }

  /** arithmetic expressions */
  case class AExpr2 (a:AExpr, op:String, b:AExpr) extends AExpr (a.toDsl+op+b.toDsl) {
    override def apply (v:Any)(implicit ctx:ECtx) = op match {
      case "+" => a(v).toString + b(v).toString
      case _ => "[ERR unknown operator "+op+"]"
    }
  }

  /** arithmetic expressions */
  class AExprIdent (val name:String) extends AExpr (name) {
    override def apply (v:Any)(implicit ctx:ECtx) = ctx.apply(name)
  }

  /** constant expression
    *
    *  TODO i'm loosing the type definition
    */
  case class CExpr (ee : String, ttype:String) extends AExpr (ee.toString) {
    override def apply (v:Any)(implicit ctx:ECtx) = ee
    override def toDsl = if(ttype == "String") ("\"" + expr + "\"") else expr
  }

}


