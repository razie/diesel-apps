package razie.diesel.dom

import razie.diesel.engine.DomEngine
import razie.diesel.ext.CanHtml
import razie.wiki.Enc

import scala.concurrent.Future

/** expression types */
object WTypes {
  final val NUMBER="Number"
  final val STRING="String"
  final val JSON="JSON"
  final val DATE="Date"
  final val REGEX="Regex"

  final val INT="Int"
  final val FLOAT="Float"
  final val BOOLEAN="Boolean"

  final val BYTES="Bytes"

  final val EXCEPTION="Exception"

  final val UNKNOWN=""

  final val appJson = "application/json"
}

/**
 * simple, neutral domain model representation: class/object/function
 *
 * These are collected in RDomain
 */
object RDOM {
  // archtetypes

  class CM // abstract Class Member
  trait DE // abstract Domain Element

  private def classLink (name:String) = s""" <b><a href="/wikie/show/Category:$name">$name</a></b> """

  /** represents a Class
    *
    * @param name         name of the class
    * @param archetype
    * @param stereotypes
    * @param base        name of base class
    * @param typeParam   if higher kind, then type params
    * @param parms       class members
    * @param methods     class methods
    * @param assocs      assocs to other classes
    * @param props       annotations and other properties
    */
  case class C (name:String, archetype:String, stereotypes:String, base:List[String], typeParam:String, parms:List[P]=Nil, methods:List[F]=Nil, assocs:List[A]=Nil, props:List[P]=Nil) extends DE {
    override def toString = fullHtml

    def fullHtml = {
      span("class::") + classLink(name) +
        smap(typeParam) (" [" + _ + "]") +
        smap(archetype) (" &lt;" + _ + "&gt;") +
        smap(stereotypes) (" &lt;" + _ + "&gt;") +
        (if(base.exists(_.size>0)) "extends " else "") + base.map("<b>" + _ + "</b>").mkString +
        mksAttrs(parms) +
        mks(methods, "{<br><hr>", "<br>", "<br><hr>}", "&nbsp;&nbsp;") +
        mks(props, " PROPS(", ", ", ") ", "&nbsp;&nbsp;")
    }

    /** combine two partial defs of the same thing */
    def plus (b:C) = {
      def combine (a:String, b:String) = {
        val res = a+","+b
        res.replaceFirst("^,", "").replaceFirst(",$", "")
      }

      val a = this

      // todo should combine / oberwrite duplicates like methods or args with same name
      C(name,
        combine(a.archetype, b.archetype),
        combine(a.stereotypes, b.stereotypes),
        a.base ++ b.base,
        combine(a.typeParam, b.typeParam),
        a.parms ::: b.parms,
        a.methods ::: b.methods,
        a.assocs ::: b.assocs,
        a.props ::: b.props
      )
    }
  }


  /** create a P with the best guess for type */
  def typified (n:String, s:Any) = {
    s match {
      case s:Float => P(n, "").withValue[Float](s, WTypes.NUMBER)
//      case s:Int => P(n, "").withValue[Int](s, "Number")
      case i: Int => P(n, i.toString, WTypes.NUMBER)
      case _ => {
        if (s.toString.trim.startsWith("{")) P(n, s.toString, WTypes.JSON)
        else P(n, s.toString)
      }
    }

  }

  // todo complete type-aware
  case class PValue[T] (value:T, contentType:String)

  type NVP = Map[String,String]

  /** represents a parameter/member/attribute
    *
    * @param name   name of parm
    * @param dflt   current value or default value
    * @param ttype  type if known
    * @param ref
    * @param multi  is this a list/array?
    * @param expr   expression - for sourced parms
    */
  case class P (name:String, dflt:String, ttype:String="", ref:String="", multi:String="", expr:Option[Expr]=None,
                var value:Option[PValue[_]] = None
               ) extends CM with razie.diesel.ext.CanHtml {

    def withValue[T](va:T, ctype:String="") = this.copy(value=Some(PValue[T](va, ctype)))

    /** proper way to get the value */
    def calculatedValue(implicit ctx: ECtx) : String =
      if(dflt.nonEmpty || expr.isEmpty) dflt else expr.get.apply ("").toString

    /** proper way to get the value */
    def calculatedTypedValue(implicit ctx: ECtx) : PValue[_] =
      value.getOrElse(
        if(dflt.nonEmpty || expr.isEmpty)
          PValue(dflt, "") // someone already calculated a non-type safe value
        else {
          val v = expr.get.applyTyped("")
          value = v.value // update computed value
          value.getOrElse(PValue(v.dflt, ""))
        }
      )

    /** me if calculated, otherwise another P, calculated */
    def calculatedP(implicit ctx: ECtx) : P =
      value.map(x=> this).getOrElse(
        if(dflt.nonEmpty || expr.isEmpty) this else {
          val v = expr.get.applyTyped("")
          v.copy(
            name=this.name,

            // interpolate type
            ttype = if(v.ttype.nonEmpty) v.ttype else expr match {
            // expression type known?
            case Some(CExpr(_, WTypes.STRING)) => WTypes.STRING
            case Some(CExpr(_, WTypes.NUMBER)) => WTypes.NUMBER
            case _ => {
              if (!expr.exists(_.getType != "") &&
                (v.value.exists(x=> x.value.isInstanceOf[Int] || x.value.isInstanceOf[Float])))
                WTypes.NUMBER
              else
                expr.map(_.getType).mkString
            }
          }
          )
        }
      )

    /** current calculated value if any or the expression */
    def valExpr = if(dflt.nonEmpty || expr.isEmpty) CExpr(dflt, ttype) else expr.get

    def strimmedDflt =
      if(dflt.size > 80) dflt.take(60) + "{...}"
      else dflt

    def htrimmedDflt =
      if(dflt.size > 80) dflt.take(60)
      else dflt

    override def toString =
      s"$name" +
        smap(ttype) (":" + ref + _) +
        smap(multi)(identity) +
        smap(strimmedDflt) (s=> "=" + (if("Number" == ttype) s else quot(s))) +
        (if(dflt=="") expr.map(x=>smap(x.toString) ("=" + _)).mkString else "")

    // todo refs for type, docs, position etc
    override def toHtml =
      s"<b>$name</b>" +
        (if(ttype.toLowerCase != "string") smap(ttype) (s=> ":" + ref + typeHtml(s)) else "") +
        smap(multi)(identity) +
        smap(Enc.escapeHtml(htrimmedDflt)) {s=>
          "=" + tokenValue(if("Number" == ttype) s else escapeHtml(quot(s)))
        } +
//        (if(dflt.length > 60) "<span class=\"label label-default\"><small>...</small></span>") +
        (if(dflt.length > 60) "<b><small>...</small></b>" else "") +
        (if(dflt=="") expr.map(x=>smap(x.toHtml) ("<-" + _)).mkString else "")

    private def typeHtml(s:String) = {
      s.toLowerCase match {
        case "string" | "number" | "date" => s"<b>$s</b>"
        case _ => classLink(s)
      }
    }
  }

  /** represents a parameter match expression
    *
    * @param name   name to match
    * @param ttype  optional type to match
    * @param ref
    * @param multi
    * @param op
    * @param dflt
    * @param expr
    */
  case class PM (name:String, ttype:String, ref:String, multi:String, op:String, dflt:String, expr:Option[Expr] = None) extends CM with CanHtml {

    /** current calculated value if any or the expression */
    def valExpr = if(dflt.nonEmpty || expr.isEmpty) CExpr(dflt, ttype) else expr.get

    override def toString =
      s"$name" +
        (if(ttype!="String") smap(ttype) (":" + ref + _) else "") +
        smap(multi)(identity) +
        smap(dflt) (s=> op + (if("Number" == ttype) s else quot(s))) +
        (if(dflt=="") expr.map(x=>smap(x.toString) (" " + op +" "+ _)).mkString else "")

    override def toHtml =
      s"<b>$name</b>" +
        (if(ttype!="String") smap(ttype) (":" + ref + _) else "") +
      smap(multi)(identity) +
      smap(dflt) (s=> op + tokenValue(if("Number" == ttype) s else quot(s))) +
        (if(dflt=="") expr.map(x=>smap(x.toHtml) (" <b>"+op +"</b> "+ _)).mkString else "")
  }

  /** a function / method
    *
    * @param name
    * @param parms
    * @param ttype
    * @param archetype def vs msg
    * @param script
    * @param body
    */
  case class F (name:String, parms:List[P], ttype:String, archetype:String, script:String="", body:List[Executable]=List.empty) extends CM with CanHtml {
    override def toHtml = "   "+  span(s"$archetype:") + s" <b>$name</b> " +
      mks(parms, " (", ", ", ") ") +
      smap(ttype) (":" + _)

    override def toString = "   "+  span(s"$archetype:") + s" <b>$name</b> " +
      mks(parms, " (", ", ", ") ") +
      smap(ttype) (":" + _)
  }

  /** an executable statement */
  trait Executable {
    def sForm:String
    override def toString = sForm
  }

  /** specific to sync executables: scripts, expressions etc */
  trait ExecutableSync extends Executable {
    def exec(ctx:Any, parms:Any*):Any
  }

  /** specific to async executables: messages, futures etc */
  trait ExecutableAsync extends Executable {
    def start(ctx: Any, inEngine:Option[DomEngine]): Future[DomEngine]
  }

  case class V (name:String, value:String)  // attr value

  case class O (name:String, base:String, parms:List[V]) { // object = instance of class
    def toJson = parms.map{p=> p.name -> p.value}.toMap
  }

  /** Diamond */
  class D  (val roles:List[(String, String)], val ac:Option[AC]=None) extends DE //diamond association

  /** name is not required */
  case class A  (name:String, a:String, z:String, aRole:String, zRole:String, parms:List[P]=Nil, override val ac:Option[AC]=None) //association
    extends D (List(a->aRole, z->zRole), ac) {
    override def toString = s"assoc $name $a:$aRole -> $z:$zRole " +
      mks(parms, " (", ", ", ") ")
  }
  case class AC (name:String, a:String, z:String, aRole:String, zRole:String, cls:C) //association class

  // Diamond Class

  case class E (name:String, parms:List[P], methods:List[F]) extends DE //event
  case class R (name:String, parms:List[P], body:String) extends DE //rule
  case class X (body:String) extends DE //expression
  case class T (name:String, parms:List[P], body:String) extends DE //pattern

  case class TYP (name:String, ref:String, kind:String, multi:String) extends DE

}
