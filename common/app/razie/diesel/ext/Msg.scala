/**
 *  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.ext

import com.mongodb.casbah.Imports._
import mod.diesel.model.DomAst
import razie.diesel.dom.RDOM._
import razie.diesel.dom._

import scala.Option.option2Iterable
import scala.collection.mutable.ListBuffer
import scala.util.Try

/** an applicable - can execute a message */
trait EApplicable {
  /** is this applicable... applicable? */
  def test(m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) : Boolean

  /** is this async?
    *
    * if not, we'll wait in this thread.
    * If Async, then it will send a DEREq to the engine when done
    */
  def isAsync : Boolean = false

  /** is this a mock? is it supposed to run in mock mode or not?
    *
    * you can have an executor for mock mode and one for normal mode
    */
  def isMock : Boolean = false

  /** do it !
    *
    * @return a list of elements - these will be wrapped in DomAst and added to the tree, so a value should be EVal etc
    */
  def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any]
}

/** test - expect a message m. optional guard */
case class ExpectM(not:Boolean, m: EMatch) extends CanHtml with HasPosition {
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
case class ExpectV(not:Boolean, pm:MatchAttrs) extends CanHtml with HasPosition {
  var when : Option[EMatch] = None
  var pos : Option[EPos] = None
  def withPos(p:Option[EPos]) = {this.pos = p; this}

  override def toHtml = kspan("expect::") +" "+ pm.map(_.toHtml).mkString("(", ",", ")")

  override def toString = "expect:: " + pm.mkString("(", ",", ")")

  def withGuard (guard:EMatch) = { this.when = Some(guard); this}
  def withGuard (guard:Option[EMatch]) = { this.when = guard; this}

  /** check to match the arguments */
  def test(a:Attrs, cole: Option[MatchCollector] = None, nodes:List[DomAst])(implicit ctx: ECtx) = {
    testA(a, pm, cole, Some({p=>
      // start a new collector to mark this value
      cole.foreach(c=>nodes.find(_.value.asInstanceOf[EVal].p.name == p.name).foreach(n=>c.newMatch(n)))
    }))
  }

  /** check to match the arguments */
  def sketch(cole: Option[MatchCollector] = None)(implicit ctx: ECtx) : Attrs = {
    sketchAttrs(pm, cole)
  }

}

// a match case
case class EMatch(cls: String, met: String, attrs: MatchAttrs, cond: Option[EIf] = None) extends CanHtml {
  // todo match also the object parms if any and method parms if any
  def test(e: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) = {
    if ("*" == cls || e.entity == cls || regexm(cls, e.entity)) {
      cole.map(_.plus(e.entity))
      if ("*" == met || e.met == met || regexm(met, e.met)) {
        cole.map(_.plus(e.met))
        testA(e.attrs, attrs, cole) && cond.fold(true)(_.test(e.attrs, cole))
      } else false
    } else false
  }

  /** extract a message signature from the match */
  def asMsg = EMsg("", cls, met, attrs.map{p=>
    // extract the sample value
    val df = if(p.dflt.nonEmpty) p.dflt else p.expr match {
      case Some(CExpr(e, _)) => e
      case _ => ""
    }
    P (p.name, df, p.ttype, p.ref, p.multi)
  })

  override def toHtml = ea(cls, met) + " " + toHtmlMAttrs(attrs)

  override def toString = cls + "." + met + " " + attrs.mkString("(", ",", ")")
}

/**
  * just a call to next - how to call next: wait => or no wait ==>
  */
case class ENext (msg:EMsg, arrow:String, cond: Option[EIf] = None) extends CanHtml {
  // todo match also the object parms if any and method parms if any
  def test(cole: Option[MatchCollector] = None)(implicit ctx: ECtx) = {
    cond.fold(true)(_.test(List.empty, cole))
  }

  override def toHtml   = arrow + " " + msg.toHtml
  override def toString = arrow + " " + msg.toString
}

/** mapping a message - a decomposition rule (right hand side of =>)
  *
  * @param cls
  * @param met
  * @param attrs
  */
case class EMap(cls: String, met: String, attrs: Attrs, arrow:String="=>", cond: Option[EIf] = None) extends CanHtml {
  var count = 0;

  def apply(in: EMsg, destSpec: Option[EMsg], pos:Option[EPos])(implicit ctx: ECtx): List[Any] = {
    var e = Try {
      val m = EMsg("generated", cls, met, sourceAttrs(in, attrs, destSpec.map(_.attrs))).
        withPos(pos).
        withSpec(destSpec)

      if(arrow == "==>" || cond.isDefined)
        ENext(m, arrow, cond)
      else m
    }.recover {
      case t:Throwable => {
        razie.Log.log("trying to source message", t)
        EError(t.getMessage, t.toString)
      }
    }.get
    count += 1

    List(e)
  }

  def sourceAttrs(in: EMsg, spec: Attrs, destSpec: Option[Attrs])(implicit ctx: ECtx) = {
    // current context, msg overrides
    val myCtx = new StaticECtx(in.attrs, Some(ctx))

    // solve an expression
    def expr(p: P) = {
      p.expr.map(_.apply("")(myCtx)/*.toString*/).getOrElse{
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

      val tt =
        if (p.ttype.isEmpty && !p.expr.exists(_.getType != "") && v.isInstanceOf[Int]) WTypes.NUMBER
        else if (p.ttype.isEmpty) p.expr.map(_.getType).mkString
        else p.ttype

      p.copy(dflt = v.toString, ttype=tt)
    } else if (destSpec.exists(_.nonEmpty)) destSpec.get.map { p =>
      // when defaulting to spec, order changes
      val v = in.attrs.find(_.name == p.name).map(_.dflt).orElse(
        ctx.get(p.name)
      ).getOrElse(
        expr(p)
      )
      val tt = if(p.ttype.isEmpty && v.isInstanceOf[Int]) WTypes.NUMBER else ""
      p.copy(dflt = v.toString, expr=None, ttype=tt)
    } else {
      // if no map rules and no spec, then just copy/propagate all parms
      in.attrs.map {a=>
        a.copy()
      }
    }
  }

  def asMsg = EMsg("", cls, met, attrs.map{p=>
    P (p.name, p.dflt, p.ttype, p.ref, p.multi)
  })

//  override def toHtml = "<b>=&gt;</b> " + ea(cls, met) + " " + attrs.map(_.toHtml).mkString("(", ",", ")")
  override def toHtml = """<span class="glyphicon glyphicon-arrow-right"></span> """ + cond.map(_.toHtml+" ").mkString + ea(cls, met) + " " + toHtmlAttrs(attrs)

  override def toString = "=> " + cond.map(_.toHtml+" ").mkString + cls + "." + met + " " + attrs.mkString("(", ",", ")")
}

import mod.diesel.model.parser.FlowExpr

// a flow
case class EFlow(e: EMatch, ex: FlowExpr) extends CanHtml with EApplicable with HasPosition {
  var pos : Option[EPos] = None
  override def test(m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) =
    e.test(m, cole)

  override def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] =
    Nil//i.apply(in, destSpec, pos)

  override def toHtml = span("$flow::") + s" ${e.toHtml} => $ex <br>"

  override def toString = s"$$flow:: $e => $ex"
}

// a context
case class ERule(e: EMatch, i: List[EMap]) extends CanHtml with EApplicable with HasPosition {
  var pos : Option[EPos] = None
  override def test(m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) =
    e.test(m, cole)

  override def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] =
    i.flatMap(_.apply(in, destSpec, pos))

  override def toHtml = span("when::", "default") + s" ${e.toHtml} ${i.map(_.toHtml).mkString("<br>")} <br>"

  override def toString = "when:: " + e + " => " + i.mkString
}

// a nvp - can be a spec or an event, message, function etc
case class EMsg(arch:String, entity: String, met: String, attrs: List[RDOM.P], ret: List[RDOM.P] = Nil, stype: String = "") extends CanHtml with HasPosition {
  var spec: Option[EMsg] = None
  var pos : Option[EPos] = None
  def withPos(p:Option[EPos]) = {this.pos = p; this}
  def withSpec(p:Option[EMsg]) = {this.spec = p; this}

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
    ) ++ {
      pos.map{p=>
        Map ("ref" -> p.toRef,
          "pos" -> p.toJmap
        )
      }.getOrElse(Map.empty)
    }

  // if this was an instance and you know of a spec
  def first: String = spec.map(_.first).getOrElse(
    kspan("msg:", resolved, spec.flatMap(_.pos)) + span(stype, "info")
  )

  /** if has executor */
  def hasExecutor:Boolean = Executors.all.exists(_.test(this)(ECtx.empty))

  def isResolved:Boolean = if(spec.exists(_.isResolved)) true else (
    if ((stype contains "GET") || (stype contains "POST") || hasExecutor ) true
    else false
  )

  /** color - if has executor */
  private def resolved: String = if(isResolved) "default" else "warning"

  /** extract a match from this message signature */
  def asMatch = EMatch(entity, met, attrs.filter(_.dflt != "").map {p=>
    PM (p.name, p.ttype, p.ref, p.multi, "==", p.dflt)
  })

  /** this html works well in a diesel fiddle, use toHtmlInPage elsewhere */
  override def toHtml = {
  /*span(arch+"::")+*/first + s""" ${ea(entity,met)} """ + toHtmlAttrs(attrs)
  }

  /** as opposed to toHtml, this will produce an html that can be displayed in any page, not just the fiddle */
  def toHtmlInPage = hrefBtn2+hrefBtn1 + toHtml.replaceAllLiterally("weref", "wefiddle")

  def hrefBtn2 =
    s"""<a href="${url2("")}" class="btn btn-xs btn-primary" title="global link">
       |<span class="glyphicon glyphicon glyphicon-th-list"></span></a>""".stripMargin
  def hrefBtn1 =
    s"""<a href="${url1("")}" class="btn btn-xs btn-info"    title="local link in this topic">
       |<span class="glyphicon glyphicon-list-alt"></span></a>""".stripMargin

  override def toString =
    s""" $entity.$met (${attrs.mkString(", ")})"""

  def attrsToUrl (attrs: Attrs) = {
    attrs.map{p=>
      s"""${p.name}=${p.dflt}"""
    }.mkString("&")
  }

  // local invocation url
  def url1 (section:String="", resultMode:String="value") = {
    var x = s"""/diesel/wreact/${pos.map(_.wpath).mkString}/react/$entity/$met?${attrsToUrl(attrs)}"""
    if (x.endsWith("&") || x.endsWith("?")) ""
    else if (x contains "?") x = x + "&"
    else x = x + "?"
    x = x + "resultMode="+resultMode

    if(section != "") {
      x = x + "&dfiddle=" + section
    }
    x
  }

  // reactor invocation url
  def url2 (section:String="", resultMode:String="value") = {
    var x = s"""/diesel/fiddle/react/$entity/$met?${attrsToUrl(attrs)}"""
    if (x.endsWith("&") || x.endsWith("?")) ""
    else if (x contains "?") x = x + "&"
    else x = x + "?"
    x = x + "resultMode="+resultMode

    if(section != "") {
      x = x + "&dfiddle=" + section
    }
    x
  }

  def toHref (section:String="", resultMode:String="debug") = {
    var u = url1(section, resultMode)
    s"""<a target="_blank" href="$u">$entity.$met(${attrs.mkString(", ")})</a>"""
  }

  def toHrefWith (text:String, section:String="", resultMode:String="debug") = {
    var u = url1(section, resultMode)
    s"""<a target="_blank" href="$u">$text</a>"""
  }
}

// a simple condition
case class EIf(attrs: MatchAttrs) extends CanHtml {
  def test(e:Attrs, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) =
    testA(e, attrs, cole)

  override def toHtml = span("$if::") + attrs.mkString("<small>(", ", ", ")</small>")

  override def toString = "$if " + attrs.mkString
}

// a wrapper
case class EMock(rule: ERule) extends CanHtml with HasPosition {
  var pos : Option[EPos] = rule.pos
  override def toHtml = span(count.toString) + " " + rule.toHtml

  override def toString = count.toString + " " + rule.toString

  def count = rule.i.map(_.count).sum  // todo is slow
}

// just a wrapper for type
case class EVal(p: RDOM.P) extends CanHtml with HasPosition {
  def this(name:String, value:String) = this(P(name, value))

  var pos : Option[EPos] = None
  def withPos(p:Option[EPos]) = {this.pos = p; this}

  def toj =
    Map (
      "class" -> "EVal",
      "name" -> p.name,
      "value" -> p.dflt
    ) ++ {
      pos.map{p=>
        Map ("ref" -> p.toRef,
         "pos" -> p.toJmap
        )
      }.getOrElse(Map.empty)
    }

  override def toHtml   = kspan("val::") + p.toHtml
  override def toString = "val: " + p.toString
}

// can execute messages -
// todo can these add more decomosition or just proces leafs?
abstract class EExecutor (val name:String) extends EApplicable {
  def messages : List[EMsg] = Nil
}

object Executors {
  val _all = new ListBuffer[EExecutor]()

  def all : List[EExecutor] = _all.toList

  def add (e:EExecutor) = {_all append e}
}

  /** some error, with a message and details */
  case class EError (msg:String, details:String="") extends CanHtml {
    override def toHtml =
    if(details.length > 0)
      span("error::", "danger", details, "style=\"cursor:help\"") + " " + msg
    else
      span("error::", "danger", details) + " " + msg
    override def toString = "error::"+msg
  }

  /** a simple info node with a message and details - details are displayed as a popup */
  case class EInfo (msg:String, details:String="") extends CanHtml with HasPosition {
    var pos : Option[EPos] = None
    def withPos(p:Option[EPos]) = {this.pos = p; this}

    override def toHtml =
      if(details.length > 0)
        span("info::", "info", details, "style=\"cursor:help\"") + " " + msg
      else
        span("info::", "info", details) + " " + msg
    override def toString = "info::"+msg
  }
