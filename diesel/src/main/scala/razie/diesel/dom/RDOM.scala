/**
  *  ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.dom

import org.json.{JSONArray, JSONObject}
import razie.diesel.engine.nodes.CanHtml
import razie.diesel.engine.{DomEngine, EContent}
import razie.diesel.expr._
import razie.js
import razie.wiki.Enc
import scala.collection.mutable.HashMap
import scala.concurrent.Future

/**
 * simple, neutral domain model representation: class/object/function/value etc
 *
 * These are collected in RDomain
 */
object RDOM {
  // archtetypes

  trait DE // abstract base class for Domain Elements (classes etc)
  class CM // abstract Class Member

  private def classLink (s:String):String = s"""<b><a href="/wikie/show/Category:$s">$s</a></b>"""

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
  case class C (
    name:String,
    archetype:String,
    stereotypes:String,
    base:List[String],
    typeParam:String,
    parms:List[P]=Nil,
    methods:List[F]=Nil,
    assocs:List[A]=Nil,
    props:List[P]=Nil) extends DE {

    override def toString = fullHtml

    def fullHtml = {
      span("class::") + classLink(name) +
        smap(typeParam) (" [" + _ + "]") +
        smap(archetype) (" &lt;" + _ + "&gt;") +
        smap(stereotypes) (" &lt;" + _ + "&gt;") +
        (if(base.exists(_.size>0)) "extends " else "") + base.map(classLink).mkString +
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

  /** name value pair */
  type NVP = Map[String,String]

  object PValue {
    // todo deprecate and remove
    @deprecated
    def apply[T] (v:T, c:String) : PValue[T] = PValue(v, WType(c))
//    def apply[T] (v:T, c:WType) : PValue[T] = PValue(v, c)
  }

  /** a basic typed value
    *
    * @param value - the actual value. Use the "asXXX" methods instead of typecasting yourself
    * @param cType - the type of content
    * @tparam T
    *
    * the asXXX methods assume it is of the right type
    */
  case class PValue[+T] (value:T, cType:WType = WTypes.wt.UNKNOWN) {
//    case class PValue[+T] (value:T, contentType:String = WTypes.UNKNOWN, domClassName:String = WTypes.UNKNOWN) {
    var cacheString : Option[String] = None

    /** @deprecated */
    def contentType = cType.name

    def asObject : collection.Map[String,Any] = asJson

    def asJson : collection.Map[String,Any] = {
      if (value.isInstanceOf[String]) razie.js.parse(value.toString)
      else value.asInstanceOf[collection.Map[String, Any]]
    }

    def asArray : collection.Seq[Any] = {
      if(! value.isInstanceOf[collection.Seq[Any]]) {
        razie.Log.error("Value is not array - will throw ClassCast asap - value is: " + this)
      }
      value.asInstanceOf[collection.Seq[Any]]
    }

    def asRange : Range = value.asInstanceOf[Range]

    def asThrowable : Throwable = value.asInstanceOf[Throwable]

    def asInt : Int = value.toString.toInt

    def asBoolean : Boolean = value.toString.toBoolean

    /** this will be parsed as JS, so escape things properly */
    def asEscapedJSString : String = {
      cType match {
        case WTypes.wt.STRING => "\"" + asString + "\""
        case _ => asString
      }
    }

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

    override def equals(obj: Any) = {
      obj match {
        case PValue(v, t) => this.cType.equals(t) && this.value.equals(t)
      }
    }
  }

  /** a generic parm source - use it to dynamically source parms from a sub-object like "diesel.xxx" or else */
  trait ParmSource {
    def remove (name: String): Option[P]
    def getp   (name: String): Option[P]
    def put    (p: P): Unit
    def listAttrs: List[P]
  }

  /** parm-related helpers
    * @deprecated
    */
  object P {
    def of(name: String, v: Any): P = fromSmartTypedValue(name, v)

    def fromSmartTypedValue(name: String, v: Any): P = v match {
      case s: String if s.trim.startsWith("{") => P.fromTypedValue(name, s, WTypes.JSON)
      case s: String if s.trim.startsWith("[") => P.fromTypedValue(name, s, WTypes.ARRAY)
      case _ => P.fromTypedValue(name, v)
    }

    def fromTypedValue(name: String, v: Any, expectedType: String): P =
      fromTypedValue(name, v, WType(expectedType))

    /** construct proper typed values */
    def fromTypedValue(name:String, v:Any, expectedType:WType=WTypes.wt.UNKNOWN):P = {

      // like an orElse - important to preserve higher types info from expected
      def expOrElse(wt:WType) = if (expectedType == WTypes.wt.UNKNOWN) wt else {
        if(expectedType.name != wt.name)
          throw new DieselExprException(s"Expected types don't match: $expectedType vs $wt")

        expectedType
      }

      val res = v match {
        case i: P           => i.copy(name=name)
        case i: PValue[_] =>   P(name, asString(i.value), i.cType).withValue(i.value, i.cType)
        case i: Boolean =>     P(name, asString(i), WTypes.wt.BOOLEAN).withValue(i, WTypes.wt.BOOLEAN)
        case i: Int =>         P(name, asString(i), WTypes.wt.NUMBER).withValue(i, WTypes.wt.NUMBER)
        case i: Long =>        P(name, asString(i), WTypes.wt.NUMBER).withValue(i, WTypes.wt.NUMBER)
        case f: Float =>       P(name, asString(f), WTypes.wt.NUMBER).withValue(f, WTypes.wt.NUMBER)
        case d: Double =>      P(name, asString(d), WTypes.wt.NUMBER).withValue(d, WTypes.wt.NUMBER)
        case d: Throwable =>   P(name, d.getMessage, WTypes.wt.EXCEPTION).withValue(d, WTypes.wt.EXCEPTION)

        case s: ParmSource =>  P(name, "Source", WTypes.wt.SOURCE).withValue(s, WTypes.wt.SOURCE)

        case i: java.lang.Integer =>
          P(name, asString(i), WTypes.wt.NUMBER).withValue(i.longValue, WTypes.wt.NUMBER)
        case i: java.lang.Boolean =>
          P(name, asString(i), WTypes.wt.BOOLEAN).withValue(i.booleanValue, WTypes.wt.BOOLEAN)
        case i: java.lang.Float =>
          P(name, asString(i), WTypes.wt.NUMBER).withValue(i.floatValue, WTypes.wt.NUMBER)
        case i: java.lang.Double =>
          P(name, asString(i), WTypes.wt.NUMBER).withValue(i.doubleValue, WTypes.wt.NUMBER)
        case i: java.lang.Long =>
          P(name, asString(i), WTypes.wt.NUMBER).withValue(i.longValue, WTypes.wt.NUMBER)

          // must be before Seq
        case r: Range =>       P(name, asString(r), WTypes.wt.RANGE).withValue(r, WTypes.wt.RANGE)
        // the "" dflt will force usage of value
        case s: collection.Map[_, _] =>         P(name, "", expOrElse(WTypes.wt.JSON)).withValue(s, expOrElse(WTypes.wt.JSON))
        case s: collection.mutable.Map[_, _] => P(name, "", expOrElse(WTypes.wt.JSON)).withValue(s, expOrElse(WTypes.wt.JSON))
        case s: collection.Seq[_] =>      P(name, "", expOrElse(WTypes.wt.ARRAY)).withValue(s, expOrElse(WTypes.wt.ARRAY))
        case s: JSONObject =>  P(name, "", expOrElse(WTypes.wt.JSON)).withValue(js.fromObject(s), expOrElse(WTypes.wt.JSON))
        case s: JSONArray =>   P(name, "", expOrElse(WTypes.wt.ARRAY)).withValue(js.fromArray(s), expOrElse(WTypes.wt.ARRAY))

        case s: String =>      {
          expectedType match {
            case WType(WTypes.JSON,_,_,_, _)  => P(name, s, expectedType).withCachedValue(js.fromObject(new JSONObject(s)), expectedType, s)
            case WType(WTypes.ARRAY,_,_,_, _) => P(name, s, expectedType).withCachedValue(js.fromArray(new JSONArray(s)), expectedType, s)
            case WType(WTypes.BOOLEAN,_,_,_, _) => P(name, s, expectedType).withCachedValue(s.toBoolean, expectedType, s)
            case WType(WTypes.NUMBER,_,_,_, _) => P(name, s, expectedType).withCachedValue(s.toFloat, expectedType, s)
            case WType(WTypes.STRING,_,_,_, _) => P(name, s, expectedType).withCachedValue(s, expectedType, s)
            case WType(WTypes.EXCEPTION,_,_,_, _) => P(name, s, expectedType)
            case _ if expectedType.trim.length > 0 =>
              throw new DieselExprException(s"$expectedType is an unknown type")
            case _ => P(name, s, WTypes.wt.STRING).withCachedValue(s, WTypes.wt.STRING, s)
          }
        }

        // java object - it's better to create this yourself
        case x@_ if expectedType == WTypes.OBJECT => P(name, "", expectedType).withValue(v, expectedType)

        case x@_ => P(name, x.toString, WTypes.wt.UNKNOWN)
      }

      // assert expected type if given
      if(expectedType != WTypes.UNKNOWN && expectedType != "" && res.ttype != expectedType)
        throw new DieselExprException(s"$name of type ${res.ttype} not of expected type $expectedType")

      res
    }

    /** value recognized as simple type? */
    def isSimpleType (value:Any):Boolean = {
      val res = value match {
        case i: Boolean =>     true
        case i: Int =>         true
        case i: Long =>        true
        case f: Float =>       true
        case d: Double =>      true
        case i: java.lang.Integer => true
        case i: java.lang.Boolean => true
        case i: java.lang.Float => true
        case i: java.lang.Double => true
        case i: java.lang.Long => true
        case s: String => true
        case s: PValue[_] => isSimpleType(s.value)
        case _ => false
      }

      res
    }

    /** nicer type-aware toString */
    def asString (value:Any) = {
      val res = value match {
        case s: HashMap[_, _] => if(s.isEmpty) "{}" else js.tojsons(s, 2).trim
          // this must be before Seq
        case r: Range => {
          "" +
              (if(r.start == scala.Int.MinValue) "" else r.start.toString) +
              ".." +
              (if(r.end == scala.Int.MaxValue) "" else r.end.toString)
        }
        case s: collection.Map[_, _] => if(s.isEmpty) "{}" else js.tojsons(s, 2).trim
        case s: collection.Seq[_] => js.tojsons(s, 0).trim
        case s: JSONObject => if(s.length() == 0) "{}" else s.toString(2).trim
        case s: JSONArray => if(s.length() == 0) "[]" else s.toString.trim
        case x@_ => x.toString
      }

      res
    }

    @deprecated
    def apply (name:String, dflt:String, ttype:String):P = P(name, dflt, WType(ttype)).withValue(dflt, WType(ttype))
  }

  //  implicit def toWtype2(s:String) : WType = WType(s)
  def wt(s:String) : WType = WType(s)
  implicit def wttos(wt: WType) : String = wt.toString

  /** represents a parameter/member/attribute
    *
    * use .calculatedTypedValue instead of accessing value/dflt/expr directly
    *
    * The value trumps dflt which trumps expr
    *
    * @param name     name of parm
    * @param dflt     current value (for values) or default value (for specs) or a cache for the typed .value
    * @param ttype    type if known
    * @param expr     expression - for sourced parms
    * @param optional "?" if the parm is optional
    */
  case class P (name:String, dflt:String, ttype:WType = WTypes.wt.EMPTY, expr:Option[Expr]=None, optional:String="",
                var value:Option[PValue[_]] = None
               ) extends CM with CanHtml {

    def withValue[T](va:T, ctype:WType = WTypes.wt.UNKNOWN) = {
      this.copy(ttype=ctype, value=Some(PValue[T](va, ctype)))
    }

    def withCachedValue[T](va:T, ctype:WType, cached:String) = {
      this.copy(ttype=ctype, value=Some(PValue[T](va, ctype).withStringCache(cached)))
    }

    def isRef = ttype.isRef

    /** check if this value or def is of type t */
    def isOfType(t:WType) = {
      value.map(_.cType == t).getOrElse(ttype == t)
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
            case Some(CExpr(_, WTypes.wt.STRING)) => WTypes.wt.STRING
            case Some(CExpr(_, WTypes.wt.NUMBER)) => WTypes.wt.NUMBER
            case _ => {
              if (!expr.exists(_.getType != "") &&
                (v.value.exists(x=> x.value.isInstanceOf[Int] || x.value.isInstanceOf[Float])))
                WTypes.wt.NUMBER
              else
                WType(expr.map(_.getType).mkString)
            }
          }
          )
        }
      )

    /** only if it was already calculated... */
    def hasCurrentValue = value.isDefined || dflt.length > 0

    /** only if it was already calculated... */
    def currentValue =
      value.map(v=> CExpr(v.value, v.cType)).getOrElse {
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
      value.map(v=> CExpr(v.value, v.cType)).getOrElse {
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
        ttype +
        optional +
        smap(strimmedDflt) (s=> "=" + (if("Number" == ttype) s else quot(s))) +
        (if(dflt=="") expr.map(x=>smap(x.toString) ("=" + _)).mkString else "")

    // todo refs for type, docs, position etc
    override def toHtml = toHtml(true)

    def toHtml (shorten:Boolean = true)=
      s"<b>$name</b>" +
        (if(ttype.name.toLowerCase != "string") typeHtml(ttype) else "") +
    optional +
        smap(Enc.escapeHtml(if(shorten) htrimmedDflt else currentStringValue)) {s=>
          "=" + tokenValue(if("Number" == ttype) s else escapeHtml(quot(s)))
        } +
//        (if(dflt.length > 60) "<span class=\"label label-default\"><small>...</small></span>") +
        (if(shorten && currentStringValue.length > 60) "<b><small>...</small></b>" else "") +
        (if(dflt=="") expr.map(x=>smap(x.toHtml) ("<-" + _)).mkString else "")

    private def typeHtml(s:WType) = {
      s.name.toLowerCase match {
        case "string" | "number" | "date" => s"<b>$s</b>"
        case _ => WTypes.mkString(s, classLink)
      }
    }
  }

  /** represents a parameter match expression
    *
    * @param ident  name to match
    * @param ttype  optional type to match
    * @param op     operation to amtch with
    * @param dflt   value to match
    * @param expr   expression to match
    */
  case class PM (ident:AExprIdent, ttype:WType, op:String,
                 dflt:String, expr:Option[Expr] = None, optional:String = "") extends CM with CanHtml {

    def name : String = ident.start

    /** current calculated value if any or the expression */
    def valExpr = if(dflt.nonEmpty || expr.isEmpty) CExpr(dflt, ttype) else expr.get

    def isMatch = ident.start.nonEmpty //&& ident.rest.isEmpty - a.b.c is fine
    def isOptional = "?" == optional

    def check (in:P)(implicit ctx: ECtx) = {
      val pm = this

      // match simple names - look at testA for complex evaluators
      in.name == pm.name && pm.ident.rest.isEmpty && {
        val r = new BCMP2(in.valExpr, pm.op, pm.valExpr).bapply("").value

        // no need to look for ?= - it's handled  above

        if (r && pm.op == "~=") {
          // for regex matches, use each capture group and set as parm in context
          // extract parms
          val a = in.valExpr.apply("")
          val b = pm.valExpr.apply("")
          val groups = EContent.extractRegexParms(b.toString, a.toString)

          groups.foreach(t => ctx.put(P(t._1, t._2)))
        } else if (r && pm.op == "~path") {
          // extract parms - special path mapping
          val a = in.valExpr.apply("")
          val b = pm.valExpr.apply("")
          val (is, groups) = EContent.extractPathParms(a.toString, b.toString)

          if(is) groups.foreach(t => ctx.put(P(t._1, t._2)))
        }

        if (!r) {
          // todo name found but no value match - mark the name
        }
        r
      }
    }

    /** if PM is just a cond, nothing to match - use this */
    def checkAsCond ()(implicit ctx: ECtx) = {
      val pm = this
        if(! expr.isDefined) throw new DieselExprException("PMatch without name needs condition expr")
        if(! expr.get.isInstanceOf[BoolExpr]) throw new DieselExprException("PMatch condition expr needs to be boolean, when no name is matched")
        expr.get.asInstanceOf[BoolExpr].bapply("", ctx)
    }

    override def toString =
      s"$ident" +
        ttype +
        optional +
        smap(dflt) (s=> op + (if("Number" == ttype) s else quot(s))) +
        (if(dflt=="") expr.map(x=>smap(x.toString) (" " + op +" "+ _)).mkString else "")

    override def toHtml =
      s"<b>$ident</b>" +
      classLink(ttype) +
      optional +
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
  case class F (name:String, parms:List[P], ttype:WType, archetype:String, script:String="",
                body:List[Executable]=List.empty) extends CM with CanHtml {

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

  /** Associations
    *
    * name is not required
    */
  case class A  (name:String, a:String, z:String, aRole:String, zRole:String, parms:List[P]=Nil, override val ac:Option[AC]=None) //association
    extends D (List(a->aRole, z->zRole), ac) {
    override def toString = s"assoc $name $a:$aRole -> $z:$zRole " +
      mks(parms, " (", ", ", ") ")
  }

  /** Association class */
  case class AC (name:String, a:String, z:String, aRole:String, zRole:String, cls:C) //association class

  // Diamond Class

  case class E (name:String, parms:List[P], methods:List[F]) extends DE //event
  case class R (name:String, parms:List[P], body:String) extends DE //rule
  case class X (body:String) extends DE //expression
  case class T (name:String, parms:List[P], body:String) extends DE //pattern

  case class TYP (name:String, ref:String, kind:String, multi:String) extends DE

}
