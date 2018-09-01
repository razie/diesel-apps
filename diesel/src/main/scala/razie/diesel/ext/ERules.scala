/**
  * ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package razie.diesel.ext

import razie.diesel.dom.RDOM._
import razie.diesel.dom._
import razie.diesel.engine.InfoNode
import razie.wiki.Enc

/** test - expect a message m. optional guard */
case class ExpectM(not: Boolean, m: EMatch) extends CanHtml with HasPosition {
  var when: Option[EMatch] = None
  var pos: Option[EPos] = None
  var target: Option[DomAst] = None // if target then applies only in that sub-tree, otherwise guessing scope

  def withPos(p: Option[EPos]) = {
    this.pos = p; this
  }

  // clone because the original is a spec, reused in many stories
  def withTarget(p: Option[DomAst]) = {
    val x = this.copy();
    x.pos = this.pos;
    x.when = this.when;
    x.target = p;
    x
  }

  override def toHtml = kspan("expect::") + " " + m.toHtml

  override def toString = "expect:: " + m.toString

  def withGuard(guard: EMatch) = {
    this.when = Some(guard); this
  }

  def withGuard(guard: Option[EMatch]) = {
    this.when = guard; this
  }

  /** check to match the arguments */
  def sketch(cole: Option[MatchCollector] = None)(implicit ctx: ECtx): List[EMsg] = {
    var e = EMsg(m.cls, m.met, sketchAttrs(m.attrs, cole), AstKinds.GENERATED)
    e.pos = pos
    //      e.spec = destSpec
    //      count += 1
    List(e)
  }
}

// todo use a EMatch and combine with ExpectM - empty e/a
/** test - expect a value or more. optional guard */
case class ExpectV(not: Boolean, pm: MatchAttrs, cond: Option[EIf] = None) extends CanHtml with HasPosition {
  var when: Option[EMatch] = None
  var pos: Option[EPos] = None
  var target: Option[DomAst] = None // if target then applies only in that sub-tree, otherwise guessing scope

  def withPos(p: Option[EPos]) = {
    this.pos = p; this
  }

  // clone because the original is a spec, reused in many stories
  def withTarget(p: Option[DomAst]) = {
    val x = this.copy()
    x.pos = this.pos  // need to copy vars myself
    x.when = this.when
    x.target = p
    x
  }

  override def toHtml =
    kspan("expect::") + " " + toHtmlMAttrs(pm) + cond.map(_.toHtml).mkString

  override def toString =
    "expect:: " + pm.mkString("(", ",", ")") + cond.map(_.toHtml).mkString

  def withGuard(guard: EMatch) = {
    this.when = Some(guard); this
  }

  def withGuard(guard: Option[EMatch]) = {
    this.when = guard; this
  }

  /** check to match the arguments */
  def applicable(a: Attrs)(implicit ctx: ECtx) = {
    cond.fold(true)(_.test(a, None))
  }

  /** check to match the arguments */
  def test(a: Attrs, cole: Option[MatchCollector] = None, nodes: List[DomAst])(implicit ctx: ECtx) = {
    testA(a, pm, cole, Some({ p =>
      // start a new collector to mark this value
      cole.foreach(c => nodes.find(_.value.asInstanceOf[EVal].p.name == p.name).foreach(n => c.newMatch(n)))
    }))
    // we don't check the cond - it just doesn't apply
    // && cond.fold(true)(_.test(a, cole))
  }

  /** check to match the arguments */
  def sketch(cole: Option[MatchCollector] = None)(implicit ctx: ECtx): Attrs = {
    sketchAttrs(pm, cole)
  }

}

/** test - expect a value or more. optional guard */
case class ExpectAssert(not: Boolean, exprs: List[BExpr]) extends CanHtml with HasPosition {
  var when: Option[EMatch] = None
  var pos: Option[EPos] = None
  var target: Option[DomAst] = None // if target then applies only in that sub-tree, otherwise guessing scope

  def withPos(p: Option[EPos]) = {
    this.pos = p; this
  }

  // clone because the original is a spec, reused in many stories
  def withTarget(p: Option[DomAst]) = {
    val x = this.copy(); x.target = p; x
  }

  override def toHtml = kspan(pos.mkString+"assert::") + " " + exprs.map(_.toDsl).mkString("(", ",", ")")

  override def toString = "assert:: " + exprs.mkString("(", ",", ")")

  def withGuard(guard: EMatch) = {
    this.when = Some(guard); this
  }

  def withGuard(guard: Option[EMatch]) = {
    this.when = guard; this
  }

  /** check to match the arguments */
  def test(a: Attrs, cole: Option[MatchCollector] = None, nodes: List[DomAst])(implicit ctx: ECtx) = {
    exprs.foldLeft(true)((a,b)=> a && b.apply("") )
//    testA(a, pm, cole, Some({ p =>
      // start a new collector to mark this value
//      cole.foreach(c => nodes.find(_.value.asInstanceOf[EVal].p.name == p.name).foreach(n => c.newMatch(n)))
//    }))
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
  def asMsg = EMsg(cls, met, attrs.map { p =>
    // extract the sample value
    val df = if (p.dflt.nonEmpty) p.dflt else p.expr match {
      case Some(CExpr(e, _)) => e
      case _ => ""
    }
    P(p.name, df, p.ttype, p.ref, p.multi)
  })

  override def toHtml = ea(cls, met) + " " + toHtmlMAttrs(attrs) + cond.map(_.toHtml).mkString

  override def toString = cls + "." + met + " " + attrs.mkString("(", ",", ")") + cond.map(_.toString).mkString
}

/** just a call to next.
  *
  * This is used to wrap async spawns ==> and
  * normal => when there's more than one (they start one at a time)
  *
  * @param msg the message wrapped / to be executed next
  * @param arrow - how to call next: wait => or no wait ==>
  * @param cond optional condition for this step
  */
case class ENext(msg: EMsg, arrow: String, cond: Option[EIf] = None, deferred:Boolean=false) extends CanHtml {
  var parent:Option[EMsg] = None
  var spec:Option[EMsg] = None

  def withParent(p:EMsg) = { this.parent=Some(p); this}
  def withSpec(p:Option[EMsg]) = { this.spec=p; this}

  // todo match also the object parms if any and method parms if any
  def test(cole: Option[MatchCollector] = None)(implicit ctx: ECtx) = {
    cond.fold(true)(_.test(List.empty, cole))
  }

  def evaluateMsg(implicit ctx: ECtx) = {
    // if evaluation was deferred, do it
    val m = if (deferred) {
      parent.map { parent =>
        msg.copy(attrs = EMap.sourceAttrs(parent, msg.attrs, spec.map(_.attrs))).withPos(msg.pos)
      } getOrElse {
        msg // todo evaluate something here as well...
      }
    } else msg

    m
  }

  override def toHtml = arrow + " " + msg.toHtml

  override def toString = arrow + " " + msg.toString
}

/** $when - match and decomposition rule
  *
  * @param e the match that triggers this rule
  * @param arch archetype or tags
  * @param i the mappings to execute
  */
case class ERule(e: EMatch, arch:String, i: List[EMap]) extends CanHtml with EApplicable with HasPosition {
  var pos: Option[EPos] = None

  override def test(m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) =
    e.test(m, cole)

  override def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] =
    i.flatMap(_.apply(in, destSpec, pos))

  override def toHtml = span(arch+"::", "default") + s" ${e.toHtml} <br>${i.map(_.toHtml).mkString("<br>")} <br>"

  override def toString = arch+":: " + e + " => " + i.mkString
}

// a simple condition
trait EIf extends CanHtml {
  def test(e: Attrs, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) : Boolean
}

// a simple condition
case class EIfm(attrs: MatchAttrs) extends CanHtml with EIf {
  def test(e: Attrs, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) =
    testA(e, attrs, cole)

  override def toHtml = span("$ifm::") + attrs.mkString("<small>(", ", ", ")</small>")

  override def toString = "$ifm " + attrs.mkString
}

// a simple condition
case class EIfc(cond: BExpr) extends CanHtml with EIf {
  def test(e: Attrs, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) =
    cond.apply("")

  override def toHtml = span("$ifc::") + cond.toDsl

  override def toString = "$ifc " + cond.toDsl
}

// $mock
case class EMock(rule: ERule) extends CanHtml with HasPosition {
  var pos: Option[EPos] = rule.pos

  override def toHtml = span(count.toString) + " " + rule.toHtml//.replaceFirst("when", "mock")

  override def toString = count.toString + " " + rule.toString//.replaceFirst("when", "mock")

  def count = rule.i.map(_.count).sum // todo is slow
}

// just a wrapper for type
case class EVal(p: RDOM.P) extends CanHtml with HasPosition {
  def this(name: String, value: String) = this(P(name, value))

  var pos: Option[EPos] = None

  def withPos(p: Option[EPos]) = {
    this.pos = p; this
  }

  def toj : Map[String,Any] =
    Map(
      "class" -> "EVal",
      "name" -> p.name,
      "value" -> p.dflt
    ) ++ {
      pos.map { p =>
        Map("ref" -> p.toRef,
          "pos" -> p.toJmap
        )
      }.getOrElse(Map.empty)
    }

  override def toHtml =
    if(pos.isDefined) kspan("val") + p.toHtml
    else spanClick("val", "info", Enc.escapeHtml(p.dflt)) + p.toHtml

  override def toString = "val " + p.toString
}

/** some error, with a message and details */
case class EWarning(msg: String, details: String = "") extends CanHtml with InfoNode {
  override def toHtml =
    if (details.length > 0)
      span("warning", "warning", details, "style=\"cursor:help\"") + " " + msg
    else
      span("warning", "warning", details) + " " + msg

  override def toString = "warning: " + "x"//msg
}

object EErrorUtils {
  def ttos (t:Throwable) = {
    razie.Log.log("error snakking", t)

    val sw = new java.io.StringWriter()
    val pw = new java.io.PrintWriter(sw)
    t.printStackTrace(pw)

    val s = sw.toString
    s
  }
}

/** some error, with a message and details */
case class EError(msg: String, details: String = "") extends CanHtml with HasPosition with InfoNode {
  def this(msg:String, t:Throwable) =
    this(msg + t.toString, EErrorUtils.ttos(t))

  var pos: Option[EPos] = None

  def withPos(p: Option[EPos]) = {
    this.pos = p; this
  }

  override def toHtml =
    if (details.length > 0)
      spanClick("fail-error::", "danger", details) + msg
//      span("error::", "danger", details, "style=\"cursor:help\"") + " " + msg
    else
      span("fail-error::", "danger", details) + " " + msg

  override def toString = "fail-error::" + msg
}

/** error and stop engine */
case class EEngStop(msg: String, details: String = "") extends CanHtml with HasPosition with InfoNode {

  var pos: Option[EPos] = None

  def withPos(p: Option[EPos]) = {
    this.pos = p; this
  }

  override def toHtml =
    if (details.length > 0)
      span("error::", "danger", details, "style=\"cursor:help\"") + " " + msg
    else
      span("error::", "danger", details) + " " + msg

  override def toString = "error::" + msg
}

/** a simple info node with a message and details - details are displayed as a popup */
case class EInfo(msg: String, details: String = "") extends CanHtml with HasPosition with InfoNode {
  var pos: Option[EPos] = None

  def withPos(p: Option[EPos]) = {
    this.pos = p; this
  }

  override def toHtml =
    if (details.length > 0) {
      spanClick(
        "info::",
        "info",
        details,
        msg
      )
    } else {
      span("info::", "info", details) + " " + msg
    }

  override def toString = "info::" + msg
}

/** duration of the curent op */
case class EDuration(millis:Long, msg: String="") extends CanHtml with InfoNode {

  override def toHtml = span("info::", "info") + s" $millis ms - $msg"

  override def toString = "info::" + s" $millis ms - $msg"
}

