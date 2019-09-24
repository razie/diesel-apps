package razie.diesel.dom

import org.json.{JSONArray, JSONObject}
import razie.diesel.engine.DomEngine
import razie.diesel.expr.{AExprIdent, CExpr, DieselExprException, Expr}
import razie.diesel.ext.CanHtml
import razie.js
import razie.wiki.Enc
import scala.concurrent.Future

/** expression and data types
  *
  * note these are physical or base types.
  */
object WTypes {
  final val NUMBER="Number"
  final val STRING="String"
  final val DATE="Date"
  final val REGEX="Regex"

  final val INT="Int"
  final val FLOAT="Float"
  final val BOOLEAN="Boolean"

  final val RANGE="Range"

  final val HTML="HTML" // why not... html templates?

  final val XML="JSON"
  final val JSON="JSON" // see asJson
  final val OBJECT="Object" // same as json

  final val ARRAY="Array" //pv.asArray

  final val BYTES="Bytes"

  final val EXCEPTION="Exception"

  final val UNKNOWN=""

  final val UNDEFINED="Undefined" // same as null

  final val appJson = "application/json"

  final val MSG = "Msg" // a message (to call)
  final val FUNC = "Func" // a function (to call)

  // see also P.fromTypedValue
  def typeOf (x:Any) = {
    val t = x match {
      case m: Map[_, _] => JSON
      case s: String => STRING
      case i: Int => NUMBER
      case f: Double => NUMBER
      case f: Float => NUMBER
      case l: List[_] => ARRAY
      case l: JSONArray => ARRAY
      case l: JSONObject => JSON
//      case l: EMsg => MSG
      case h @ _ => UNKNOWN
    }

    t
  }

  /** get corresponding content-type */
  def getContentType (ttype:String) = {
    val t = ttype match {
      case JSON | ARRAY => appJson
      case HTML => "text/html"
      case h @ _ => "text/plain"
    }

    t
  }
}

/**
 * simple, neutral domain model representation: class/object/function
 *
 * These are collected in RDomain
 */
object RDOM {
  // archtetypes

  class CM // abstract Class Member
  trait DE // abstract base class for Domain Elements (classes etc)

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
        mksAttrs(parms, Some({p:P =>
          "<small>" + qspan("", p.name) + "</small> " + p.toHtml
        })) +
        mks(methods, "{<br><hr>", "<br>", "<br><hr>}", "&nbsp;&nbsp;") +
        mks(props, " PROPS(", ", ", ") ", "&nbsp;&nbsp;")
    }

    def qspan(s: String, p:String, k: String = "default") = {
      def mkref: String = s"weDomQuery('d365odata', 'default', '$name', '$p');"
      s"""<span onclick="$mkref" style="cursor:pointer" class="label label-$k"><i class="glyphicon glyphicon-search" style="font-size:1"></i></span>&nbsp;"""
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


  type NVP = Map[String,String]

  /** a basic typed value
    *
    * @param value - the actual value. Use the "asXXX" methods instead of typecasting yourself
    * @param contentType - the type of content
    * @param domClassName - the dom type = classname or domain.class
    * @tparam T
    *
    * the asXXX methods assume it is of the right type
    */
  case class PValue[T] (value:T, contentType:String = WTypes.UNKNOWN, domClassName:String = WTypes.UNKNOWN) {
    var cacheString : Option[String] = None

    def asObject : Map[String,Any] = asJson

    def asJson : Map[String,Any] = {
      if (value.isInstanceOf[String]) razie.js.parse(value.toString)
      else value.asInstanceOf[Map[String, Any]]
    }

    def asArray : List[Any] = value.asInstanceOf[List[Any]]

    def asRange : Range = value.asInstanceOf[Range]

    def asThrowable : Throwable = value.asInstanceOf[Throwable]

    def asInt : Int = value.toString.toInt

    def asBoolean : Boolean = value.toString.toBoolean

    /** cached, nicer type-aware toString */
    def asString = {
      if (cacheString.nonEmpty) cacheString.get
      else {
        cacheString = Some(P.asString(value))
        cacheString.get
      }
    }

    /** non-cached, nice type-aware toString */
    def asNiceString = {
      P.asString(value)
    }

    def withStringCache (s:String) = {
      val x = this.copy()
      x.cacheString = Some(s)
      x
    }
  }

  /** parm-related helpers */
  object P {
    /** construct proper typed values */
    def fromTypedValue(name:String, v:Any, expectedType:String=WTypes.UNKNOWN) = {
      val res = v match {
        case i: Boolean =>     P(name, asString(i), WTypes.BOOLEAN).withValue(i, WTypes.BOOLEAN)
        case i: Int =>         P(name, asString(i), WTypes.NUMBER).withValue(i, WTypes.NUMBER)
        case f: Float =>       P(name, asString(f), WTypes.NUMBER).withValue(f, WTypes.NUMBER)
        case d: Double =>      P(name, asString(d), WTypes.NUMBER).withValue(d, WTypes.NUMBER)

          // the "" dflt will force usage of value
        case s: Map[_, _] =>   P(name, "", WTypes.JSON).withValue(s, WTypes.JSON)
        case s: List[_] =>     P(name, "", WTypes.ARRAY).withValue(s, WTypes.ARRAY)
        case s: JSONObject =>  P(name, "", WTypes.JSON).withValue(js.fromObject(s), WTypes.JSON)
        case s: JSONArray =>   P(name, "", WTypes.ARRAY).withValue(js.fromArray(s), WTypes.ARRAY)

        case r: Range =>       P(name, asString(r), WTypes.RANGE).withValue(r, WTypes.RANGE)
        case s: String =>      {
          expectedType match {
            case WTypes.JSON  => P(name, s, WTypes.JSON).withCachedValue(js.fromObject(new JSONObject(s)), WTypes.JSON, s)
            case WTypes.ARRAY => P(name, s, WTypes.ARRAY).withCachedValue(js.fromArray(new JSONArray(s)), WTypes.ARRAY, s)
            case WTypes.BOOLEAN => P(name, s, WTypes.BOOLEAN).withCachedValue(s.toBoolean, WTypes.BOOLEAN, s)
            case WTypes.NUMBER => P(name, s, WTypes.NUMBER).withCachedValue(s.toFloat, WTypes.NUMBER, s)
            case _ if expectedType.trim.length > 0 =>
              throw new DieselExprException(s"$expectedType is an unknown type")
            case _ => P(name, asString(s), WTypes.STRING)
          }
        }
        case x@_ =>            P(name, x.toString, WTypes.UNKNOWN)
      }

      // assert expected type if given
      if(expectedType != WTypes.UNKNOWN && expectedType != "" && res.ttype != expectedType)
        throw new DieselExprException(s"$name of type ${res.ttype} not of expected type $expectedType")

      res
    }

    /** nicer type-aware toString */
    def asString (value:Any) = {
      val res = value match {
        case s: Map[_, _] => js.tojsons(s, 2).trim
        case s: List[_] => js.tojsons(s, 0).trim
        case s: JSONObject => s.toString(2).trim
        case s: JSONArray => s.toString.trim
        case r: Range => {
          "" +
              (if(r.start == scala.Int.MinValue) "" else r.start.toString) +
              ".." +
              (if(r.end == scala.Int.MaxValue) "" else r.end.toString)
        }
        case x@_ => x.toString
      }

      res
    }
  }

  /** represents a parameter/member/attribute
    *
    * use .calculatedTypedValue instead of accessing value/dflt/expr directly
    *
    * The value trumps dflt which trumps expr
    *
    * @param name   name of parm
    * @param dflt   current value (for values) or default value (for specs) or a cache for the typed .value
    * @param ttype  type if known
    * @param ref
    * @param multi  is this a list/array?
    * @param expr   expression - for sourced parms
    */
  case class P (name:String, dflt:String, ttype:String="", ref:String="", multi:String="", expr:Option[Expr]=None,
                var value:Option[PValue[_]] = None
               ) extends CM with razie.diesel.ext.CanHtml {

    def withValue[T](va:T, ctype:String="", domClassName:String=WTypes.UNKNOWN) = {
      this.copy(ttype=ctype, value=Some(PValue[T](va, ctype, domClassName)))
    }

    def withCachedValue[T](va:T, ctype:String="", cached:String) = {
      this.copy(ttype=ctype, value=Some(PValue[T](va, ctype, WTypes.UNKNOWN).withStringCache(cached)))
    }

    def isRef = {
      ref != ""
    }

    /** proper way to get the value */
    def calculatedValue(implicit ctx: ECtx) : String =
      // important to avoid toString JSONS all the time...
      if(value.isDefined) value.get.asString // this will cache the string
      else if(dflt.nonEmpty || expr.isEmpty) dflt else {
        calculatedTypedValue.asString
      }

    /** proper way to get the value */
    def calculatedTypedValue(implicit ctx: ECtx) : PValue[_] =
      value.getOrElse(
        if(dflt.nonEmpty || expr.isEmpty) {
          PValue(currentStringValue, ttype) // someone already calculated a value, maybe a ttype as well...
        } else {
          val v = expr.get.applyTyped("")
          value = v.value // update computed value
          value.getOrElse(PValue(v.currentStringValue, ""))
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

    /** only if it was already calculated... */
    def hasCurrentValue = value.isDefined || dflt.length > 0

    /** only if it was already calculated... */
    def currentValue =
      value.map(v=> CExpr(v.value, v.contentType)).getOrElse {
        CExpr(dflt, ttype)
      }

    /** only if it was already calculated... */
    def currentStringValue =
    // not looking at dflt - the value is cached as string too
//      if(dflt.nonEmpty) dflt
      value.map(_.asString).getOrElse {
        dflt
      }

    /** current calculated value if any or the expression */
    def valExpr =
      value.map(v=> CExpr(v.value, v.contentType)).getOrElse {
        if(dflt.nonEmpty || expr.isEmpty) CExpr(dflt, ttype) else expr.get
      }

    def strimmedDflt = {
      val d = currentStringValue
      if(d.size > 80) d.take(60) + "{...}"
      else d
    }

    def htrimmedDflt = {
      val d = currentStringValue
      if(d.size > 80) d.replaceAll("\n", "").take(60)
      else d
    }

    override def toString =
      s"$name" +
        smap(ttype) (":" + ref + _) +
        smap(multi)(identity) +
        smap(strimmedDflt) (s=> "=" + (if("Number" == ttype) s else quot(s))) +
        (if(dflt=="") expr.map(x=>smap(x.toString) ("=" + _)).mkString else "")

    // todo refs for type, docs, position etc
    override def toHtml = toHtml(true)

    def toHtml (shorten:Boolean = true)=
      s"<b>$name</b>" +
        (if(ttype.toLowerCase != "string") smap(ttype) (s=> ":" + ref + typeHtml(s)) else "") +
        smap(multi)(identity) +
        smap(Enc.escapeHtml(if(shorten) htrimmedDflt else currentStringValue)) {s=>
          "=" + tokenValue(if("Number" == ttype) s else escapeHtml(quot(s)))
        } +
//        (if(dflt.length > 60) "<span class=\"label label-default\"><small>...</small></span>") +
        (if(shorten && currentStringValue.length > 60) "<b><small>...</small></b>" else "") +
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
  case class PM (ident:AExprIdent, ttype:String, ref:String, multi:String, op:String, dflt:String, expr:Option[Expr] = None) extends CM with CanHtml {

    def name : String = ident.start

    /** current calculated value if any or the expression */
    def valExpr = if(dflt.nonEmpty || expr.isEmpty) CExpr(dflt, ttype) else expr.get

    override def toString =
      s"$ident" +
        (if(ttype!="String") smap(ttype) (":" + ref + _) else "") +
        smap(multi)(identity) +
        smap(dflt) (s=> op + (if("Number" == ttype) s else quot(s))) +
        (if(dflt=="") expr.map(x=>smap(x.toString) (" " + op +" "+ _)).mkString else "")

    override def toHtml =
      s"<b>$ident</b>" +
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

  /** object = instance of class  - we're not really using this, but P, for values
    *
    * @param name
    * @param base
    * @param parms
    */
  case class O (name:String, base:String, parms:List[P]) {
    def toJson = parms.map{p=> p.name -> p.value}.toMap

    def fullHtml = {
      span("object::") + " " + name + " : " + classLink(base) +
//        smap(typeParam) (" [" + _ + "]") +
//        smap(archetype) (" &lt;" + _ + "&gt;") +
//        smap(stereotypes) (" &lt;" + _ + "&gt;") +
//        (if(base.exists(_.size>0)) "extends " else "") + base.map("<b>" + _ + "</b>").mkString +
        mksAttrs(parms, Some({p:P =>
          p.toHtml(false) +
            (
              if(p.isRef)
                s""" <small><a href="/diesel/objBrowserById/d365odata/default/${p.ttype}/${p.currentStringValue}">browse</a></small>"""
              else
                ""
              )
        })) +
//        mks(methods, "{<br><hr>", "<br>", "<br><hr>}", "&nbsp;&nbsp;") +
//        mks(props, " PROPS(", ", ", ") ", "&nbsp;&nbsp;")
      ""
    }

    /** get a nice display name - this assumes you merged it with its class, see OdataCrmDomainPlugin.oFromJ */
    def getDisplayName = {
      // todo use class def to find key parms
      parms.filter(p=> p.name.contains("name") || p.name.contains("key")).map {p=>
        p.name + "=" + p.currentStringValue
      }.mkString(" , ")
    }
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
