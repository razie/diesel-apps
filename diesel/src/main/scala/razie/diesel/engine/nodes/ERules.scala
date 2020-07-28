/*  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine.nodes

import razie.{cdebug, clog, ctrace}
import razie.diesel.dom.RDOM._
import razie.diesel.dom._
import razie.diesel.engine.exec.EApplicable
import razie.diesel.engine.{AstKinds, DomAst}
import razie.diesel.expr.{BoolExpr, CExpr, ECtx}
import razie.tconf.EPos
import razie.wiki.Enc

/** test - expect a message m. optional guard */
case class ExpectM(not: Boolean, m: EMatch) extends CanHtml with HasPosition {
  var when: Option[EMatch] = None
  var pos: Option[EPos] = None
  var target: Option[DomAst] = None // if target then applies only in that sub-tree, otherwise guessing scope

  // todo implement the cond

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
    kspan("expect::") + (if(not) "NOT" else "") + " " + toHtmlMAttrs(pm) + cond.map(_.toHtml).mkString

  override def toString =
    "expect:: " + (if(not) "NOT" else "") + " " + pm.mkString("(", ",", ")") + cond.map(_.toHtml).mkString

  def withGuard(guard: Option[EMatch]) = {
    this.when = guard; this
  }

  /** check to match the arguments */
  def applicable(a: Attrs)(implicit ctx: ECtx) = {
    cond.fold(true)(_.test(a, None))
  }

  /** check to match the arguments
    *
    * @param a results of previous tree/message or Nil for context-only matches
    * @param cole collector for debugging info
    * @param nodes nodes that were targeted, to reference in collectors
    * @param ctx
    */
  def test(a: Attrs, cole: Option[MatchCollector] = None, nodes: List[DomAst])(implicit ctx: ECtx) = {
    val res = testA(a, pm, cole, Some({ p =>
      // start a new collector for each value we're looking for, to mark this value
      cole.foreach{c=>
        nodes
          .find(_.value.asInstanceOf[EVal].p.name == p.name)
          .foreach(n => c.newMatch(n))
        }
    }), !not)
    // we don't check the cond - it just doesn't apply
    // && cond.fold(true)(_.test(a, cole))
    res
  }

  /** check to match the arguments */
  def sketch(cole: Option[MatchCollector] = None)(implicit ctx: ECtx): Attrs = {
    sketchAttrs(pm, cole)
  }

}

/** test - expect a value or more. optional guard */
case class ExpectAssert(not: Boolean, exprs: List[BoolExpr]) extends CanHtml with HasPosition {
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
    // todo collect from bapply
    exprs.foldLeft(true)((a,b)=> a && b.bapply("").value )
//    testA(a, pm, cole, Some({ p =>
      // start a new collector to mark this value
//      cole.foreach(c => nodes.find(_.value.asInstanceOf[EVal].p.name == p.name).foreach(n => c.newMatch(n)))
//    }))
  }
}

// a match case
case class EMatch(cls: String, met: String, attrs: MatchAttrs, cond: Option[EIf] = None) extends CanHtml {
  // todo match also the object parms if any and method parms if any
  /** test if this rule should apply
    * @param fallback if this is looking for fallbacks or not
    */
  def test(e: EMsg, cole: Option[MatchCollector] = None, fallback:Boolean = false)(implicit ctx: ECtx) = {
    if(testEA(e, cole, fallback))
      testAttrCond(e, cole, fallback)
    else false
  }

  /** test that the EA matches */
  def testEA(e: EMsg, cole: Option[MatchCollector] = None, fallback:Boolean = false)(implicit ctx: ECtx) = {
//    cdebug << s"Rule tested: $this"
    if(cls.length == 0 || met.length == 0) {
      regexm(cls+met, e.ea) // full match
    } else if ("*" == cls || e.entity == cls || regexm(cls, e.entity)) {
      cole.map(_.plus(e.entity))
      if ("*" == met || e.met == met || regexm(met, e.met)) {
        cole.map(_.plus(e.met))
        true
      } else {
//        ctrace << s"...rule skipped EA: $this"
        false
      }
    } else false
  }

  /** test that the attrs and cond match */
  def testAttrCond(e: EMsg, cole: Option[MatchCollector] = None, fallback:Boolean = false)(implicit ctx: ECtx) = {
        cole.map(_.plus(e.met))
        val testedok = testMatchAttrs(e.attrs, attrs, cole)
        val condok = if(testedok) true else cond.fold(true)(_.test(e.attrs, cole)) // respect bool shortcut
        fallback || {
          if(! (testedok && condok)) ctrace << s"...rule skipped attr/cond: $this"
          testedok && condok
        }
  }

  /** extract a message signature from the match */
  def asMsg = EMsg(cls, met, attrs.map { p =>
    // extract the sample value
    val df = if (p.dflt.nonEmpty) p.dflt else p.expr match {
      case Some(CExpr(e, _)) => e.toString
      case _ => ""
    }
    P(p.name, df, p.ttype)
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
  * @param deferred
  */
case class ENextPas(msg: EMsgPas, arrow: String, cond: Option[EIf] = None, deferred:Boolean=false, indentLevel:Int=0) extends CanHtml {
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
        EMap.sourcePasAttrs(msg.attrs)
    } else msg

    m
  }

  override def toHtml = (if (arrow != "-") arrow + " " else "") + msg.toHtml

  override def toString = (if (arrow != "-") arrow + " " else "") + msg.toString
}

/** something that needs to decompose later, in another decomp cycle - the basis for async execution
  *
  * This is used to wrap async spawns ==>
  * and normal => when there's more than one (they start one at a time) so then decomp is async
  *
  * @param msg the message wrapped / to be executed next
  * @param arrow - how to call next: wait => or no wait ==>
  * @param cond optional condition for this step
  * @param deferred
  * @param indentLevel if > 0 then this is part of a subtree
  */
case class ENext(msg: EMsg, arrow: String, cond: Option[EIf] = None, deferred:Boolean=false, indentLevel:Int=0) extends CanHtml {
  var parent:Option[EMsg] = None
  var spec:Option[EMsg] = None

  def withParent(p:EMsg) = { this.parent=Some(p); this}
  def withSpec(p:Option[EMsg]) = { this.spec=p; this}

  // todo match also the object parms if any and method parms if any
  def test(cole: Option[MatchCollector] = None)(implicit ctx: ECtx) = {
    cond.fold(true)(_.test(List.empty, cole))
  }

  /** apply - evaluate the message if not already */
    def evaluateMsg(implicit ctx: ECtx) = {
      // if evaluation was deferred, do it
      val m = if (deferred) {
        parent.map { parent =>
          msg
              .copy(attrs = EMap.sourceAttrs(parent, msg.attrs, spec.map(_.attrs)))
              .copiedFrom(msg)
        } getOrElse {
          msg // todo evaluate something here as well...
        }
      } else msg

      m
    }

  override def toHtml = (if (arrow != "-") arrow + " " else "") + msg.toHtml

  override def toString = (if (arrow != "-") arrow + " " else "") + msg.toString
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

  /** after testing, apply this rule and decomp the message */
  override def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] = {
    in.withRulePos(pos) // set my pos on decomposed msg - I must have matched it
    i.flatMap(_.apply(in, destSpec, pos, false, arch))
  }

  override def toHtml =
    span(arch+"::", "default") +
        e.asMsg.hrefBtnGlobal +
        s" ${e.toHtml} <br>${i.map(_.toHtml).mkString("<br>")} <br>"

  override def toString = arch+":: " + e + " => " + i.mkString
}

// base for conditions
trait EIf extends CanHtml {
  def test(e: Attrs, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) : Boolean
}

// a match condition
case class EIfm(attrs: MatchAttrs) extends CanHtml with EIf {
  def test(e: Attrs, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) =
    testMatchAttrs(e, attrs, cole)

  override def toHtml = span("$ifm::") + attrs.mkString("<small>(", ", ", ")</small>")
  override def toString = "$ifm " + attrs.mkString
}

// an expression condition
case class EIfc(cond: BoolExpr) extends CanHtml with EIf {
  def test(e: Attrs, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) =
    // todo collect from bapply
    cond.bapply("").value

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

  // overwrite default, so some values don't show in info view
  var kind: Option[String] = None
  def withKind(k: String) = {
    this.kind = Some(k); this
  }

  def toj : Map[String,Any] =
    Map(
      "class" -> "EVal",
      "name" -> p.name,
      "value" -> p.currentStringValue
    ) ++ {
      pos.map { p =>
        Map("ref" -> p.toRef,
          "pos" -> p.toJmap
        )
      }.getOrElse(Map.empty)
    }

  override def toHtml =
    if(pos.isDefined) kspan("val") + p.toHtml
    else spanClick("val", "info", Enc.escapeHtml(p.currentStringValue)) + p.toHtml

  override def toString = "val " + p.toString
}

