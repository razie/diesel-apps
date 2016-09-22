/**
 *  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.ext

import razie.db.RazSalatContext._
import razie.db._
import com.mongodb.casbah.Imports._
import com.mongodb.DBObject
import com.novus.salat._
import org.joda.time.DateTime

import razie.diesel.dom._
import razie.diesel.dom.RDOM._
import razie.diesel._
import razie.js

import scala.Option.option2Iterable
import scala.collection.mutable
import scala.collection.mutable.{HashMap, ListBuffer}

trait EApplicable {
  def test(m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) : Boolean

  def isMock : Boolean = false

  def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any]
}

/** test - expect a message m. optional guard */
case class ExpectM(m: EMatch) extends CanHtml with HasPosition {
  var when : Option[EMatch] = None
  var pos : Option[EPos] = None
  def withPos(p:Option[EPos]) = {this.pos = p; this}

  override def toHtml = kspan("expect::") +" "+ m.toHtml

  override def toString = "expect:: " + m.toString

  def withGuard (guard:EMatch) = { this.when = Some(guard); this}
  def withGuard (guard:Option[EMatch]) = { this.when = guard; this}

  /** check to match the arguments */
  def sketch(cole: Option[MatchCollector] = None)(implicit ctx: ECtx) : List[EMsg] = {
    var e = EMsg("generated", m.cls, m.met, sketchAttrs(m.attrs, cole))
    e.pos = pos
    //      e.spec = destSpec
    //      count += 1
    List(e)
  }
}

// todo use a EMatch and combine with ExpectM - empty e/a
/** test - expect a value or more. optional guard */
case class ExpectV(pm:MatchAttrs) extends CanHtml with HasPosition {
  var when : Option[EMatch] = None
  var pos : Option[EPos] = None
  def withPos(p:Option[EPos]) = {this.pos = p; this}

  override def toHtml = kspan("expect::") +" "+ pm.map(_.toHtml).mkString("(", ",", ")")

  override def toString = "expect:: " + pm.mkString("(", ",", ")")

  def withGuard (guard:EMatch) = { this.when = Some(guard); this}
  def withGuard (guard:Option[EMatch]) = { this.when = guard; this}

  /** check to match the arguments */
  def test(a:Attrs, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) = {
    testA(a, pm, cole)
  }

  /** check to match the arguments */
  def sketch(cole: Option[MatchCollector] = None)(implicit ctx: ECtx) : Attrs = {
    sketchAttrs(pm, cole)
  }

}

// a match case
case class EMatch(cls: String, met: String, attrs: MatchAttrs, cond: Option[EIf] = None) extends CanHtml {
  // check if it matches a regex
  def regexm(re:String, str:String) = {
    if((re startsWith "/") && (re endsWith "/")) {
      str.matches(re.substring(1,re.length-1))
    } else
      false
  }
  // todo match also the object parms if any and method parms if any
  def test(e: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) = {
    if ("*" == cls || e.entity == cls || regexm(cls, e.entity)) {
      cole.map(_.plus(e.entity))
      if ("*" == met || e.met == met || regexm(met, e.met)) {
        cole.map(_.plus(e.met))
        testA(e.attrs, attrs, cole) && cond.fold(true)(_.test(e, cole))
      } else false
    } else false
  }

  /** extract a message signature from the match */
  def asMsg = EMsg("", cls, met, attrs.map{p=>
    P (p.name, p.ttype, p.ref, p.multi, p.dflt)
  })

  override def toHtml = cls + "." + met + " " + attrs.map(_.toHtml).mkString("(", ",", ")")

  override def toString = cls + "." + met + " " + attrs.mkString("(", ",", ")")
}

// mapping a message
case class EMap(cls: String, met: String, attrs: Attrs) {
  var count = 0;

  def apply(in: EMsg, destSpec: Option[EMsg], pos:Option[EPos])(implicit ctx: ECtx): List[Any] = {
    var e = EMsg("generated", cls, met, sourceAttrs(in, attrs, destSpec.map(_.attrs)))
    e.pos = pos
    e.spec = destSpec
    count += 1
    List(e)
  }

  def sourceAttrs(in: EMsg, spec: Attrs, destSpec: Option[Attrs])(implicit ctx: ECtx) = {
    // current context, msg overrides
    val myCtx = new StaticECtx(in.attrs, Some(ctx))

    // solve an expression
    def expr(p: P) = {
      p.expr.map(_.apply("")(myCtx).toString).getOrElse{
        val s = p.dflt
        s
        //          if (s.matches("[0-9]+")) s // num
        //          else if (s.startsWith("\"")) s // string
        //          else in.attrs.find(_.name == s).map(_.dflt).getOrElse("")
      }
    }

    if (spec.nonEmpty) spec.map { p =>
      // sourcing has expr, overrules
      val v =
        if(p.dflt.length > 0 || p.expr.nonEmpty) expr(p)
        else in.attrs.find(_.name == p.name).map(_.dflt).orElse(
          ctx.get(p.name)
        ).getOrElse(
          "" // todo or somehow mark a missing parm?
        )
      p.copy(dflt = v)
    } else if (destSpec.exists(_.nonEmpty)) destSpec.get.map { p =>
      // when defaulting to spec, order changes
      val v = in.attrs.find(_.name == p.name).map(_.dflt).orElse(
        ctx.get(p.name)
      ).getOrElse(
        expr(p)
      )
      p.copy(dflt = v, expr=None)
    } else Nil
  }

  def asMsg = EMsg("", cls, met, attrs.map{p=>
    P (p.name, p.ttype, p.ref, p.multi, p.dflt)
  })

  override def toString = cls + "." + met + " " + attrs.mkString
}

// a context
case class ERule(e: EMatch, i: EMap) extends CanHtml with EApplicable with HasPosition {
  var pos : Option[EPos] = None
  override def test(m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) =
    e.test(m, cole)

  override def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] =
    i.apply(in, destSpec, pos)

  override def toHtml = span("$when::") + " " + e + " => " + i

  override def toString = "$when:: " + e + " => " + i
}

// a nvp - can be a spec or an event, message, function etc
case class EMsg(arch:String, entity: String, met: String, attrs: List[RDOM.P], ret: List[RDOM.P] = Nil, stype: String = "") extends CanHtml with HasPosition {
  var spec: Option[EMsg] = None
  var pos : Option[EPos] = None
  def withPos(p:Option[EPos]) = {this.pos = p; this}

  def toj =
    Map (
      "class" -> "EMsg",
      "arch" -> arch,
      "entity" -> entity,
      "met" -> met,
      "attrs" -> attrs.map { p =>
        Map(
          "name" -> p.name,
          "value" -> p.dflt
        )
      },
      "ret" -> ret.map { p =>
        Map(
          "name" -> p.name,
          "value" -> p.dflt
        )
      },
      "stype" -> stype
    )

  // if this was an instance and you know of a spec
  def first: String = spec.map(_.first).getOrElse(
    kspan("msg:", resolved, spec.flatMap(_.pos)) + span(stype, "info")
  )

  /** color - if has executor */
  private def resolved: String = spec.map(_.resolved).getOrElse(
    if (stype == "GET" || stype == "POST" ||
      Executors.all.exists(_.test(this)(ECtx.empty))
    ) "default"
    else "warning"
  )

  /** extract a match from this message signature */
  def asMatch = EMatch(entity, met, attrs.filter(_.dflt != "").map {p=>
    PM (p.name, p.ttype, p.ref, p.multi, "==", p.dflt)
  })

  override def toHtml =
  /*span(arch+"::")+*/first + s""" $entity.<b>$met</b> (${attrs.map(_.toHtml).mkString(", ")})"""

  def toHtmlInPage = hrefBtn2+hrefBtn1 + toHtml.replaceAllLiterally("weref", "wefiddle")

  def hrefBtn2 = s"""<a href="${url2("")}" class="btn btn-xs btn-primary"><span class="glyphicon glyphicon-link"></span></a>"""
  def hrefBtn1 = s"""<a href="${url1("")}" class="btn btn-xs btn-info"><span class="glyphicon glyphicon-th-list"></span></a>"""

  override def toString =
    s""" $entity.$met (${attrs.mkString(", ")})"""

  def attrsToHtml (attrs: Attrs) = {
    attrs.map{p=>
      s"""${p.name}=${p.dflt}"""
    }.mkString("&")
  }

  def url1 (section:String="") = {
    var x = s"""/diesel/wiki/${pos.map(_.wpath).mkString}/react/$entity/$met?${attrsToHtml(attrs)}"""
    if (x.endsWith("&") || x.endsWith("?")) ""
    else if (x contains "?") x = x + "&"
    else x = x + "?"
    x = x + "resultMode=debug"

    if(section != "") {
      x = x + "&dfiddle=" + section
    }
    x
  }

  def url2 (section:String="") = {
    var x = s"""/diesel/fiddle/react/$entity/$met?${attrsToHtml(attrs)}"""
    if (x.endsWith("&") || x.endsWith("?")) ""
    else if (x contains "?") x = x + "&"
    else x = x + "?"
    x = x + "resultMode=debug"

    if(section != "") {
      x = x + "&dfiddle=" + section
    }
    x
  }

  def toHref (section:String="") = {
    var u = url1(section)
    s"""<a href="$u">$entity.$met (${attrs.mkString(", ")})</a>"""
  }
}

// a simple condition
case class EIf(attrs: MatchAttrs) extends CanHtml {
  def test(e: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) =
    testA(e.attrs, attrs, cole)

  override def toHtml = span("$if::") + attrs.mkString

  override def toString = "$if " + attrs.mkString
}

// a wrapper
case class EMock(rule: ERule) extends CanHtml with HasPosition {
  var pos : Option[EPos] = None
  override def toHtml = span(count.toString) + " " + rule.toHtml

  override def toString = count.toString + " " + rule.toString

  def count = rule.i.count
}

// just a wrapper for type
case class EVal(p: RDOM.P) extends CanHtml with HasPosition {
  var pos : Option[EPos] = None
  def withPos(p:Option[EPos]) = {this.pos = p; this}

  def toj =
    Map (
      "class" -> "EVal",
      "name" -> p.name,
      "value" -> p.dflt
    )

  override def toHtml = kspan("val::") + p.toString

  override def toString = "val: " + p.toString
}

// can execute messages -
// todo can these add more decomosition or just proces leafs?
abstract class EExecutor (val name:String) extends EApplicable {
}

object Executors {
  val _all = new ListBuffer[EExecutor]()

  def all : List[EExecutor] = _all.toList

  def add (e:EExecutor) = {_all append e}
}

