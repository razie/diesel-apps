/*  ____    __    ____  ____  ____  ___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

import org.bson.types.ObjectId
import razie.audit.Audit
import razie.js
import razie.diesel.Diesel
import razie.diesel.dom.RDOM.P
import razie.diesel.dom.{RDomain, WTypes}
import razie.diesel.engine.RDExt._
import razie.diesel.engine.exec.{EApplicable, Executors}
import razie.diesel.engine.nodes.StoryNode
import razie.diesel.engine.nodes._
import razie.diesel.expr._
import razie.diesel.model.DieselMsg
import razie.diesel.model.DieselMsg.ENGINE._
import razie.hosting.Website
import razie.tconf.DSpec
import razie.wiki.Config
import scala.Option.option2Iterable
import scala.collection.mutable.{HashMap, ListBuffer}
import scala.util.Try

/** the actual engine implementation
  *
  * we expand and execute the known nodes
  */
class DomEngineV1(
  dom: RDomain,
  root: DomAst,
  settings: DomEngineSettings,
  pages: List[DSpec],
  description: String,
  correlationId: Option[String] = None,
  id: String = new ObjectId().toString)
    extends DomEngine(
      dom, root, settings, pages, description, correlationId, id
    ) {

  assert(settings.realm.isDefined, "need realm defined for engine settings")

  /** transform one element / one step
    *
    * 1. you can collect children of the current node, like info nodes etc
    * 2. return continuations, including processing through the children
    *
    * @param a       node to decompose
    * @param recurse should recurse
    * @param level
    * @return continuations for this branch, if any
    */
  protected override def expand(a: DomAst, recurse: Boolean = true, level: Int): List[DEMsg] = {
    var msgs : List[DEMsg] = Nil // continuations from this cycle

    if (level >= maxLevels) {
      evAppChildren(a,
        DomAst(
          TestResult(
            "fail: maxLevels!", "You have a recursive rule generating this branch..."),
          "error"))
      return Nil
    }

    if(this.curExpands > maxExpands) {
      // THIS IS OK: append direct to children
      a append DomAst(
        TestResult(
          "fail: maxExpands!", s"You have expanded too many nodes (>$maxExpands)..."), "error")
      return Nil //stopNow
    }

    // link the spec - some messages get here without a spec, because the DOM is not available when created
    if(a.value.isInstanceOf[EMsg] && a.value.asInstanceOf[EMsg].spec.isEmpty) {
      val m = a.value.asInstanceOf[EMsg]
      dom.moreElements.collect {
        case s: EMsg if s.entity == m.entity && s.met == m.met => m.withSpec(Some(s))
      }.headOption
    }

    a.value match {

      case stop: EEngStop => {
        // stop engine
        evAppChildren(a, DomAst(EInfo(s"""Stopping engine $href""")))//.withPos((m.get.pos)))
        stopNow
      }

      case stop: EEngSuspend => {
        // suspend and wait for a continuation async
        evAppChildren(a, DomAst(EInfo(s"""Suspending engine $href""")))
        try {
          stop.onSuspend.foreach(_.apply(this, a, level))
        } catch {
          case e: Exception => {
            msgs = rep(a, recurse, level, List(
              DomAst(new EError("While calling suspend callback", e), AstKinds.ERROR)
            ))
          }
        }
      }

      case stop: EEngComplete => {
        evAppChildren(a, DomAst(EInfo(s"""Complete suspended node... """)))
      }

      case next@ENext(m, arrow, cond, _, _) if "==>" == arrow || "<=>" == arrow => {
        // forking new engine / async branch

        implicit val ctx = a.withCtx(mkPassthroughMsgContext(
          next.parent,
          next.parent.map(_.attrs).getOrElse(Nil),
          this.ctx, // todo probably use a.getCtx.get not this.ctx
          a))

        // start new engine/process - evaluate this message as the root of new engine
        // todo should find settings for target service  ?
        val spawnedNodes = ListBuffer(DomAst(next.evaluateMsgCall))
        var correlationId = this.id

        if ("<=>" == arrow) {
          val suspend =
            DomAst(EEngSuspend("spawn <=>", "Waiting for spawned engine", Some((e, a, l) => {
            })))

          // make sure the other engine knows it has to notify me
          correlationId = correlationId + "." + suspend.id

          msgs = rep(a, recurse, level, List(suspend))
        }

        // create new engine
        val eng = spawn(spawnedNodes.toList, Some(correlationId))

        eng.process // start it up in the background

        // todo use mkLink for asset, not hardcode url
        evAppChildren(a, DomAst(EInfo(
          s"""Spawn $arrow engine ${eng.href}""")))//.withPos((m.get.pos)))

        if ("<=>" == arrow) {
        }

        // else no need to return anything - children have been decomposed
      }

      case n1@ENext(m, ar, cond, _, _) if "-" == ar || "=>" == ar => {
        // message executed later

        // todo bubu: static parent parms overwrite side effects updated in this.ctx

        implicit val ctx = a.withCtx(mkPassthroughMsgContext(
          n1.parent,
          n1.parent.map(_.attrs).getOrElse(Nil),
          a.getCtx.get, //RAZ2 this.ctx,
          a))

        if (n1.test(a)) {
          val newmsg = n1.evaluateMsgCall
          val newnode = DomAst(newmsg, AstKinds.kindOf(newmsg.arch))

          // old node may have had children already
          newnode.moveAllNoEvents(a)

          msgs = rep(a, recurse, level, List(newnode))
        } else {
          a.parent.foreach { p =>
            evAppChildren(p,
              DomAst(EInfo("$if was false: " + n1.cond.mkString).withPos(n1.msg.pos), AstKinds.TRACE))
          }
        }
        // message will be evaluate() later
      }

      // simple assignment
      case n1@ENextPas(m, ar, cond, _, _) if "-" == ar || "=>" == ar => {

        // ctx needed for the test below
        implicit val ctx = a.getCtx.get

        if (n1.test(a)) {
          // run in parent's context - no need for a local
          appendValsPas(Some(a), m.pos, Some(m), m.attrs, a.getCtx.get)
        } else {
          a.parent.foreach { p =>
            evAppChildren(p,
              DomAst(EInfo("$if was false: " + n1.cond.mkString).withPos(n1.msg.pos), AstKinds.TRACE))
          }
        }
      }

      // not so simple assignment
      case n1@EMsgPas(ar) => {

        // run in parent's context - no need for a local
        appendValsPas(Some(a), n1.pos, Some(n1), n1.attrs, a.getCtx.get)
      }

      case x: EMsg if x.entity == "" && x.met == "" => {
        implicit val ctx = a.withCtx(mkPassthroughMsgContext(
          None,
          Nil,
          a.getCtx.get, //RAZ2 this.ctx,
          a))

        // just expressions, generate values from each attribute
        appendVals(a, x.pos, Some(x), x.attrs, ctx, AstKinds.kindOf(x.arch))
      }

      case in: EMsg => {

        // todo should collect all parent contexts from root to here...
        // todo problem right now: parent parms are not in context, so can't use in child messages

        // node context created in expandEMsg
        msgs = expandEMsgAndRep(a, in, recurse, level, a.getCtx.get) ::: msgs
      }

      case n: EVal if !AstKinds.isGenerated(a.kind) => {

        implicit val ctx = a.getCtx.get
        // $val defined in scope in story, nit spec.
        val p = n.p.calculatedP // only calculate if not already calculated =likely in a different context=

        appendVals(a, n.pos, None, List(p), a.getCtx.get, AstKinds.TRACE)
      }

      case e: ExpectM => if (!settings.simMode) expandExpectM(a, e)

      case e: ExpectV => if (!settings.simMode) expandExpectV(a, e)

      case e: ExpectAssert => if (!settings.simMode) expandExpectAssert(a, e)

      case st: StoryNode => {

        // re-add the story elements
        val nodes = a.children
        nodes.foreach(_.resetParent(null))
        a.childrenCol.clear()
        msgs = rep(a, recurse, level, nodes)
      }

      case e: InfoNode =>  // ignore
      case e: EVal =>  // ignore - (a = b) are executed as EMsgPas / NextMsgPas - see appendValsPas

      // todo should execute
      case s: EApplicable => {

        clog << "EXEC KNOWN: " + s.toString
        evAppChildren(a,
          DomAst(EError("Can't EApplicable KNOWN - " + s.getClass.getSimpleName, s.toString), AstKinds.ERROR))
      }

      case s@_ => {

        clog << "NOT KNOWN: " + s.toString
        evAppChildren(a, DomAst(
          EWarning("NODE NOT KNOWN - " + s.getClass.getSimpleName, s.toString),
          AstKinds.GENERATED
        ))
      }
    }
    msgs
  }

  /** expand a single message */
  protected override def expandEMsg(a: DomAst, in: EMsg, recurse: Boolean, level: Int, parentCtx: ECtx):
  (List[DomAst], List[DomAst])
  = {
    var newNodes: List[DomAst] = Nil // nodes generated this call collect here
    var skippedNodes: List[DomAst] = Nil // nodes skipped during this call collect here

    implicit var ctx = parentCtx

    // expand message expr if the message attrs were expressions, calculate their values
    val n: EMsg = calcMsg(a, in)(ctx)
    // replace it so we see the calculated values
    a.value = n

    //todo used to set kind to generated but parent makes more sense.getOrElse(AstKinds.GENERATED)
    val parentKind = a.kind match {
      case AstKinds.TRACE => a.kind
      case _ => AstKinds.GENERATED
    }

    // this is causing BIG WEIRD problems...
//     a.children.append(DomAst(in, AstKinds.TRACE).withStatus(DomState.SKIPPED))
//    a.children.append(DomAst(EInfoWrapper(in), AstKinds.TRACE).withStatus(DomState.SKIPPED))

    // if node already has a context, just use it (stories wrapped in Scope for instance)
    ctx = if (
      a.getMyOwnCtx.exists(_.isInstanceOf[StaticECtx]) ||
          a.getMyOwnCtx.exists(_.isInstanceOf[ScopeECtx]) ||
          a.getMyOwnCtx.exists(_.isInstanceOf[DomEngECtx])) {

      a.getMyOwnCtx.get
    } else {
      ctx = mkPassthroughMsgContext(Some(in), n.attrs, parentCtx, a)
      a.withCtx(ctx)
      ctx
    }

    // PRE - did it already have some children, decomposed by someone else?
    newNodes = a.children
    // reset parents
    newNodes.foreach(_.resetParent(null))
    a.childrenCol.clear() // remove them so we don't have duplicates in tree - they will be processed later

    // 1. engine message?
    var (mocked, skipped, moreNodes) = expandEngineEMsg(a, n, newNodes)

    skippedNodes = skippedNodes ::: skipped
    newNodes = newNodes ::: moreNodes

    var mocksApplied = HashMap[String, EMock]()

    // 2. if not, look for mocks
    if (settings.mockMode) {
      val exclusives = HashMap[String, ERule]()
      var matchingRules = (root.collect {
        // mocks from story AST
        case d@DomAst(m: EMock, _, _, _) if m.rule.e.test(a, n) && a.children.isEmpty => m
      } ::: dom.moreElements.toList).collect {
        // todo perf optimize moreelements.toList above
        // plus mocks from spec dom
        case m: EMock if m.rule.e.test(a, n) && a.children.isEmpty => m
      }

      // todo do fallbacks for mocks, like in the rules?

      matchingRules
          .sortBy(0 - _.rule.arch.indexOf("exclusive")) // put excl first, so they execute and kick out others
          .map {m=>
          mocked = true
          val exKey = m.rule.e.cls + "." + m.rule.e.met
          mocksApplied.put (exKey, m)

          //filter out exclusives - if other rules with the same name applied, the exclusive will kick out the others
          if(exclusives.contains(exKey)) { // there's an exclusive for this - ignore it
            newNodes = newNodes :::
              DomAst(EInfo("mock excluded", exKey).withPos(m.pos), AstKinds.TRACE) ::
              Nil
        } else {
          if(m.rule.arch.contains("exclusive")) {
            exclusives.put (m.rule.e.cls + "." + m.rule.e.met, m.rule) // we do exclusive per pattern
          }

          // run the mock
          newNodes = newNodes ::: ruleDecomp(a, n, m.rule, ctx)
        }
      }
    }

    // 2. rules
    var ruled = false
    // no engine messages fit, so let's find rules
    // todo - WHY? only some systems may be mocked ???
    if ((true || !mocked) && !settings.simMode) {
      var matchingRules = rules
          .filter(x => !x.isFallback) // without fallbacks first
          .filter(_.e.testEA(n))
          .filter(x => x.e.testAttrCond(a, n) || {
            // add debug
            newNodes = newNodes ::: DomAst(
              EInfo("rule skipped: " + x.e.toString, x.e.toString + "\n" + x.pos).withPos(x.pos),
              AstKinds.TRACE) :: Nil
            false
          })

      val exclusives = HashMap[String, ERule]()

      // no match without fallbacks - use fallbacks?
      if (matchingRules.isEmpty)
        matchingRules =
            rules
                .filter(_.isFallback)
                .filter(_.e.testEA(n, None, true))
                .filter(x => x.e.testAttrCond(a, n, None, true) || {
                  // add debug
                  newNodes = newNodes ::: DomAst(
                    EInfo("rule skipped: " + x.e.toString, x.e.toString + "\n" + x.pos).withPos(x.pos),
                    AstKinds.TRACE) :: Nil
                  false
                })

      matchingRules
          .sortBy(0 - _.arch.indexOf("exclusive")) // put excl first, so they execute and kick out others
          .filter {r=>
            // only apply the rules when mocks did not apply for the same key
              // todo use the mock tag query on the origin of the rules
            val exKey = r.e.cls + "." + r.e.met
            !mocksApplied.contains(exKey)
          }.map { r =>
        // each matching rule
        ruled = true

        //filter out exclusives - if other rules with the same name applied, the exclusive will kick out the others
        val exKey = r.e.cls + "." + r.e.met

        if(exclusives.contains(exKey)) { // there's an exclusive for this - ignore it
          newNodes = newNodes :::
              DomAst(EInfo("rule excluded", exKey).withPos(r.pos), AstKinds.TRACE) ::
              Nil
        } else {
          if (r.isExclusive) {
            exclusives.put(r.e.cls + "." + r.e.met, r) // we do exclusive per pattern
          }

          newNodes = newNodes ::: ruleDecomp(a, n, r, ctx)
        }
      }
    }

    /* any generated sub-messages go in sequence - important as many mocks and rules may apply
       side-effecting

       this applies across all rules and mocks decomposed, in sequence

       this will chain all generated messages, so each waits for predecessor

      we only apply default sync if no applicable $flow
     */
    if (findFlows(a).isEmpty && newNodes.size > 1) newNodes.drop(1).foldLeft(newNodes.head)((a, b) => {
      b.withPrereq(List(a.id))
    })

    // 3. executors

    // no mocks, let's try executing it
    // todo WHY - I run snaks even if rules fit - but not if mocked
    // todo now I run only if nothing else fits
    if (!mocked && !ruled && !settings.simMode) {
      // todo can I just take the first executor that works?
      Executors.withAll(_.filter { x =>
        // todo inconsistency: I am running rules if no mocks fit, so I should also run
        //  any executor ??? or only the isMocks???
        (true /*!settings.mockMode || x.isMock*/) && x._2.test(a, n)
      }.map { t =>
        val r = t._2
        mocked = true

        val news = try {

          // call the executor
          val xx = r.apply(n, None)

          // step 1: expand lists
          val yy = xx.flatMap(x => {
            x match {
              case l: List[_] => l
              case e@_ => List(e)
            }
          })

          yy.collect{
            // collect resulting values in the context as well
            case v@EVal(p) => {
              setSmartValueInContext(a, ctx, v)
              v
            }
            case p:P => {
              val v = EVal(p)
              this.setSmartValueInContext(a, ctx, v)
              v
            }
            case e@_ => e
          }.map{x =>
            // and now wrap in AST
            (
                if((x.isInstanceOf[DieselTrace]))
                  x.asInstanceOf[DieselTrace].toAst
                else
                  DomAst(x,
                    (
                        if (x.isInstanceOf[EInfo]) AstKinds.DEBUG
                        else if (x.isInstanceOf[ETrace]) AstKinds.TRACE
                        else if (x.isInstanceOf[EDuration]) AstKinds.TRACE
                        else if (x.isInstanceOf[EVal]) x.asInstanceOf[EVal].kind.getOrElse(parentKind)
                        else if (x.isInstanceOf[EError]) AstKinds.ERROR
                        else parentKind
                        )
                  )
                ).withSpec(r)
          }
        } catch {
          case e: Throwable =>
            razie.Log.alarmThis("Exception from Executor:", e)
            val p = EVal(P(Diesel.PAYLOAD, e.getMessage, WTypes.wt.EXCEPTION).withValue(e, WTypes.wt.EXCEPTION))

            setSmartValueInContext(a, ctx, p.p)
            List(DomAst(new EError("Exception:", e), AstKinds.ERROR), DomAst(p, AstKinds.ERROR))
        }

        /* make any generated activities dependent so they run in sequence
        * todo how to hangle decomp to parallel? */
        if (findFlows(a).isEmpty && news.size > 1) news.drop(1).foldLeft(news.head)((a, b) => {
          b.withPrereq(List(a.id))
        })

        newNodes = newNodes ::: news
      })

    // 4. sketch

    // last ditch attempt, in sketch mode: if no mocks or rules, run the expects
    if (!mocked && settings.sketchMode) {
      // sketch messages - from AST and then dom, as the stories are not always told
      // when running as an API from REST (wrest) the story is collected in DOM not in AST
      (collectValues {
        case x: ExpectM if x.when.exists(_.test(a, n)) => x
      }.headOption
          orElse
          dom.moreElements.collectFirst {
            case x: ExpectM if x.when.exists(_.test(a, n)) => x
          }
          ).map { e =>
        mocked = true

        val spec = dom.moreElements.collectFirst {
          case x: EMsg if x.entity == e.m.cls && x.met == e.m.met => x
        }

        val newctx = mkLocalMsgContext(Some(n), n.attrs, ctx, a)
        val news = e.sketch(None)(newctx).map(x => DomAst(x, AstKinds.SKETCHED).withSpec(e))
        newNodes = newNodes ::: news
      }

      // sketch messages - from AST and then dom, as the stories are not always told
      // when running as an API from REST (wrest) the story is collected in DOM not in AST
      (collectValues {
        case x: ExpectV if x.when.exists(_.test(a, n)) => x
      }.headOption
          orElse
          dom.moreElements.collectFirst {
            case x: ExpectV if x.when.exists(_.test(a, n)) => x
          }
          ).foreach { e =>
        val newctx = new StaticECtx(n.attrs, Some(ctx), Some(a))
        val news = e.sketch(None)(newctx).map(x => EVal(x)).map(x => DomAst(x, AstKinds.SKETCHED).withSpec(e))
        mocked = true

        news.foreach { n =>
          evAppChildren(a, n)
        }
      }
    }

      // nothing matched ?
      if(!mocked && newNodes.isEmpty) {
        // change the nodes' color to warning and add an ignorable warning
        import EMsg._
        // todo this is bad because I change the message - is there impact to persistence of events and DomAsts ?
        a.value match {
          case m: EMsg => //findParent(a).foreach { parent =>
            a.value = m.copy(
              stype = s"${m.stype},$WARNING"
            ).copiedFrom(m)
//            parent.children.update(parent.children.indexWhere(_ eq initialA), a)
          //}
        }

        // not for internal diesel messages
        val ms = n.entity + "." + n.met
        if(!ms.startsWith("diesel.")) {
          val cfg = this.pages.map(_.specRef.wpath).mkString("\n")
          evAppChildren(a, DomAst(
            EWarning(
              "No rules, mocks or executors match for " + in.toString,
              s"Review your engine configuration (blender=${settings.blenderMode}, mocks=${settings.blenderMode}, drafts=${settings.blenderMode}, tags), " +
                  s"spelling of messages or rule clauses / pattern matches\n$cfg",
              DieselMsg.ENGINE.ERR_NORULESMATCH),
            AstKinds.DEBUG
          ))
        }
      }
    }

    (newNodes, skippedNodes)
  }

  /** reused - execute a rule */
  private def ruleDecomp (a:DomAst, in:EMsg, r:ERule, parentCtx:ECtx) = {
    var result: List[DomAst] = Nil
    implicit val ctx: ECtx = new StaticECtx(in.attrs, Some(parentCtx), Some(a))

    val parents = new collection.mutable.HashMap[Int, DomAst]()

    //todo used to set kind to generated but parent makes more sense.getOrElse(AstKinds.GENERATED)
    val parentKind = a.kind match {
      case AstKinds.TRACE => a.kind
      case _ => AstKinds.GENERATED
    }

    def findParent(level: Int): DomAst = {
      if (level >= 0) parents.get(level).getOrElse(findParent(level - 1))
      else throw new DieselExprException("can't find parent in ruleDecomp")
    }

    // used to build trees from flat list with levels
    def addChild(x: Any, level: Int) = {
      val ast = DomAst(x, AstKinds.NEXT).withSpec(r)
      parents.put(level, ast)

      if (level == 0) Some(ast)
      else if (level > 0) {
        val parent = findParent(level - 1)
        parent.appendAllNoEvents(List(ast)) // todo I'm not creating events for persist for this
        None //child was consumed

        // the way leveled children work: when processing the parent message, they are REMOVED and re-added as
        // "generated" nodes
      } else {
        throw new DieselExprException("child level negative in ruleDecomp")
      }
    }

    var subRules = 0

    // generate each gen/map
    r.i.map { ri =>
      // find the spec of the generated message, to ref
      val spec = dom.moreElements.collectFirst {
        case x: EMsg if x.entity == ri.cls && x.met == ri.met => x
      }
      val KIND = if (parentKind != AstKinds.TRACE) AstKinds.kindOf(r.arch) else parentKind

      val generated = ri.apply(in, spec, r.pos, r.i.size > 1, r.arch)

      // defer evaluation if multiple messages generated - if just one message, evaluate values now
      val newMsgs = generated.collect {
        // hack: if only values then collect resulting values and dump message
        // only for size 1 because otherwise they may be in a sequence of messages and their value
        // may depend on previous messages and can't be evaluated now
        //
        case x: EMsg if x.entity == "" && x.met == "" && r.i.size == 1 => {
          // these go into the ctx as well
          appendVals(a, x.pos, Some(x), x.attrs, ctx, KIND)
          None
        }

        case x: EMsg if x.entity == "" && x.met == "" && r.i.size > 1 => {
          // can't execute now, but later
          Some(DomAst(ENext(x, "=>", None, true), AstKinds.NEXT).withSpec(r))
        }

        // else collect message
        case x: EMsg => {
          subRules += 1
          if(r.i.size > 1)
            Some(DomAst(ENext(x, "=>"), AstKinds.NEXT).withSpec(r))
          else
            Some(DomAst(x, KIND).withSpec(r))
        }

        case x: ENext => {
          subRules += 1
          addChild(x, x.indentLevel)
        }

        case x: ENextPas => {
          addChild(x, x.indentLevel)
        }

        case x @ _ => {
          // vals and whatever other things
          subRules += 1
          Some(DomAst(x, KIND).withSpec(r))
        }
      }.filter(_.isDefined)

      // if multiple results from a single map, default to sequence, i.e. make them dependent
      //        newMsgs.drop(1).foldLeft(newMsgs.headOption.flatMap(identity))((a,b)=> {
      //          Some(b.get.withPrereq(List(a.get.id)))
      //        })

      result = result ::: newMsgs.flatMap(_.toList)
    }

    // wrap in rule context push/pop
    // comment this if to allow local vars to propagate...

    // most likely got here with a LocalCtx ->
    // replace parent context with scoped or passthrough if just one PAS
    // this rule scope is just starting, so we have to decide what we'll use

    // I was going to allow single sets to go up, but NAH, they make it too random behaviour - all sets behave the same

    val oldCtx = a.getCtx.get.asInstanceOf[SimpleECtx]
    val newParentCtx = a.getCtx.get.base.orElse(a.getCtx)
    val replacementCtx = new RuleScopeECtx(oldCtx.cur, newParentCtx, Some(a)).replacing(oldCtx)
    a.replaceCtx(replacementCtx)

    // if has a spec and exports params, export them
    in.spec.toList.filter(_.ret.size > 0).flatMap(_.ret).foreach { r =>
      val x = EMsg(
        "ctx.export",
        List(
          P("toExport", r.name, WTypes.wt.STRING),
          P(r.name, "", WTypes.wt.EMPTY, Some(AExprIdent(r.name)))
        )
      )//, AstKinds.TRACE)
      x.withPos(in.pos)
      result = result ::: List(
        DomAst(ENext(x, "=>"), AstKinds.NEXT).withSpec(r)
      )
    }

    /* if multiple results from a single map, default to sequence, i.e. make them dependent

      this one applies to those generated from this rule only

      this will chain the children, so each waits for predecessor

      we only apply default sync if no applicable $flow
     */
    if (findFlows(a).isEmpty && result.size > 1) result.drop(1).foldLeft(result.head)((a, b) => {
      b.withPrereq(List(a.id))
    })

    result
  }

  /** if it's an internal engine message, execute it
    *
    * @param a
    * @param in
    * @param childrenIfAny children of the current node, if there were any - see call site
    * @param ctx
    * @return
    */
  private def expandEngineEMsg(a: DomAst, in: EMsg, childrenIfAny: List[DomAst])(implicit ctx: ECtx): (Boolean,
      List[DomAst], List[DomAst]) = {
    var skippedNodes = new ListBuffer[DomAst] // nodes skipped during this call collect here
    var newNodes = new ListBuffer[DomAst] // nodes skipped during this call collect here

    val ea = in.ea

    def skipNode(ast: DomAst, state: String) = {
      skippedNodes.append(ast)
      evChangeStatus(ast, state)
    }

    def addChild(parent: DomAst, ast: DomAst) = {
//      evAppChildren(parent, ast)
      newNodes.append(ast)
    }

    val res = {
      if (ea == DieselMsg.ENGINE.DIESEL_NOP) {
        Audit.logdb("DIESEL_NOP", s"user ${settings.userId}")

        true

      } else if (ea == DieselMsg.ENGINE.DIESEL_EXIT) {
        Audit.logdb("DIESEL_EXIT", s"user ${settings.userId}")

        if (Config.isLocalhost) {
          System.exit(-1)
        }

        true

      } else if (ea == DieselMsg.ENGINE.DIESEL_CRASHAKKA) {
        Audit.logdb("DIESEL_CRASHAKKA", s"user ${settings.userId}")

        if (Config.isLocalhost) {
          DieselAppContext.getActorSystem.terminate()
        }

        true

      } else if (ea == DieselMsg.ENGINE.DIESEL_RETURN) {
        // expand all spec vals
        in.attrs.map(_.calculatedP).foreach { p =>
          evAppChildren(a, DomAst(EVal(p)))
          setSmartValueInContext(a, a.getCtx.get.root, p) // set into root context, since we're returning
        }

        // story node prevents an entire test terminated because one story used diesel.return
        // 1. try to see if I'm inside a story - the top most node under the story,
        // if not, then the story and if not, then the ROOT

        var scope =
          findParentWith(a, _.parent.exists(_.value.isInstanceOf[StoryNode]))
              .orElse(
                findParentWith(a, _.value.isInstanceOf[StoryNode])
              )
              .getOrElse(root)

        // stop other children
        val skipped = scope.collect {
          case ast if (!DomState.isDone(ast.status)) => {
            if (DomState.inProgress(ast.status)) {
              skipNode(ast, DomState.CANCEL)
            } else {
              skipNode(ast, DomState.SKIPPED)
            }
          }
            ast
        }

        val newD = DomAst(new EInfo(s"Skipped ${skipped.size} nodes", ""), AstKinds.GENERATED)
        evAppChildren(a, newD)

        true

      } else if (ea == DieselMsg.ENGINE.DIESEL_SCOPE_RETURN) {
        // expand all spec vals
        in.attrs.map(_.calculatedP).foreach { p =>
          evAppChildren(a, DomAst(EVal(p)))
          // set into scope context, since we're finishing this scope
          setSmartValueInContext(a, a.getCtx.get.getScopeCtx, p)
        }

        // stop other children of scope
        val skipped = findScope(a).collect {
          case ast if (!DomState.isDone(ast.status)) => {
            if (DomState.inProgress(ast.status)) {
              skipNode(ast, DomState.CANCEL)
            } else {
              skipNode(ast, DomState.SKIPPED)
            }
          }
            ast
        }

        val newD = DomAst(new EInfo(s"Skipped ${skipped.size} nodes", ""), AstKinds.GENERATED)
        evAppChildren(a, newD)

        true

      } else if (ea == DieselMsg.ENGINE.DIESEL_RULE_RETURN) {
        // expand all spec vals
        in.attrs.map(_.calculatedP).foreach { p =>
          evAppChildren(a, DomAst(EVal(p)))
          // set into scope context, since we're finishing this scope
          setSmartValueInContext(a, a.getCtx.get.getScopeCtx, p)
        }

        // stop other children of scope
        val rule = findParentWith(a, _.getMyOwnCtx.exists(_.isInstanceOf[RuleScopeECtx]))
        val toSkip = rule.orElse(Some(findScope(a))).collect {
          // first parent may be a ENext, see through
          case ast if ast.value.isInstanceOf[ENext] => findParent(ast)
          case ast@_ => Some(ast)
        }.flatten.toList.flatMap(_.children)

        var counter = 0
        toSkip.foreach { a =>
          a.collect {
            case ast if (!DomState.isDone(ast.status)) => {
              counter += 1
              if (DomState.inProgress(ast.status)) {
                skipNode(ast, DomState.CANCEL)
              } else {
                skipNode(ast, DomState.SKIPPED)
              }
              ast
            }
          }
        }

        val newD = DomAst(new EInfo(s"Skipped ${counter} nodes", ""), AstKinds.GENERATED)
        evAppChildren(a, newD)

        true

      } else if (ea == DieselMsg.ENGINE.DIESEL_THROW) {

        // todo
        // exc has scope, exception, message and more stuff
        // default scope is global, other values: "scope" or "rest" or "local"

        // todo also while recursing see if htere's matching CATCH and stop skipping

        var msg = "diesel.thrown"
        var code: Option[Int] = Some(500) // default code

        // expand all spec vals
        in.attrs.map(_.calculatedP).foreach { p =>
          evAppChildren(a, DomAst(EVal(p)))
          setSmartValueInContext(a, a.getCtx.get.getScopeCtx, p) // set to scope directly, as we're exiting the others

          if (p.name == Diesel.PAYLOAD || p.name == "msg") {
            msg = p.currentStringValue
          } else if (p.name == DieselMsg.HTTP.STATUS) {
            code = Some(p.calculatedTypedValue.asInt)
          }
        }

        var caught = false
        val aLevel = findLevel(a)

        // stop other children
        // todo find cur scope, not parent
        findScope(a).collect {
          case ast@_ => Some(ast)
        }.flatten.toList.flatMap(_.children).foreach { ast =>
          if (
            false && !caught &&
                (
                    ast.value.isInstanceOf[EMsg] &&
                        ast.value.asInstanceOf[EMsg].ea == DieselMsg.ENGINE.DIESEL_CATCH
                    )
          //&&
//            findLevel(ast) <= aLevel // only if it's higher level or sibbling
          // todo should i also check I don't know what, to make sure it was meant to catch me?
          // todo that it comes after ME
          // todo that it didn't catch something already?
          ) {
            val x = findLevel(ast)
            caught = true // stop when caught

            val old = ast.value.asInstanceOf[EMsg]

            // todo add info to the catch that it caught this
            ast.value = old.copy(
              attrs = old.attrs ++ List(
                P.fromSmartTypedValue("caught", true),
                P.fromSmartTypedValue("exception", msg)
              )
            ).copiedFrom(old)
            // todo record this change in state for engine recovery
          } else if (
            false &&
                !caught &&
                (
                    ast.value.isInstanceOf[ENext] &&
                        ast.value.asInstanceOf[ENext].msg.ea == DieselMsg.ENGINE.DIESEL_CATCH
                    )
          //&&
//            findLevel(ast) <= aLevel // only if it's higher level or sibbling
          // todo should i also check I don't know what, to make sure it was meant to catch me?
          // todo that it comes after ME
          // todo that it didn't catch something already?
          ) {
            val x = findLevel(ast)
            caught = true // stop when caught

            val oldm = ast.value.asInstanceOf[ENext].msg
            val olde = ast.value.asInstanceOf[ENext]

            // todo add info to the catch that it caught this
            val newm = oldm.copy(
              attrs = oldm.attrs ++ List(
                P.fromSmartTypedValue("caught", true),
                P.fromSmartTypedValue("exception", msg)
              )
            ).copiedFrom(oldm)

            ast.value = olde.copy(msg = newm).copiedFrom(olde)

            // todo record this change in state for engine recovery
          } else if (!DomState.isDone(ast.status) && !caught) {
            skipNode(ast, DomState.SKIPPED)
          }
        }

        // todo then set payload to exception
        val ex = new DieselException(msg, code)

        val p = EVal(P(Diesel.PAYLOAD, msg, WTypes.wt.EXCEPTION).withValue(ex, WTypes.wt.EXCEPTION))
        setSmartValueInContext(a, a.getCtx.get.getScopeCtx, p.p) // set straight to scope

        val newD = DomAst(new EError("Exception:", ex), AstKinds.ERROR)
        evAppChildren(a, newD)

        true

      } else if (ea == DieselMsg.ENGINE.DIESEL_TRY) {
        // rudimentary TRY scope for catch.
        // normally we'd have CATCH apply to the enclosing scope, but we don't have a good EEScope

        val newD = DomAst(new EScope("diesel.try:", ""), AstKinds.GENERATED)
        evAppChildren(a, newD)
        true

      } else if (ea == DieselMsg.ENGINE.DIESEL_CATCH) {

        // todo handle
        // for the current scope, stop prpag exc and handle if matching
        // exc has attrs: exception and message and then more crap
        // catch must match

        // for now - let's just handle scope exceptions

        var inScope = false
        var done = false

        // 1. am I in scope?
        findScope(a).collect {
          case ast@_ => Some(ast)
        }.flatten.toList.flatMap(_.children).foreach { ast =>
          if (!done) {
            ast.collect({
              // todo more generic scope ident
              // the idea is that catch works in a scope, not just inside try
              case x if x.value.isInstanceOf[EMsg] && x.value.asInstanceOf[EMsg].ea == DIESEL_TRY => {
                inScope = true
              }
            })

            if (ast eq a) {
              done = true // found myself - stop - good for seq todo handle par
            }
          }
        }

        done = false

        var handledError = false

        val exc = new ListBuffer[P]()

        findScope(a).collect {
          case ast@_ => Some(ast)
        }.flatten.toList.flatMap(_.children).foreach { ast =>
          // find any EError and mark it "handled"
          if (!done) {
            ast.collect({
              case x if x.value.isInstanceOf[EError] && !x.value.asInstanceOf[EError].handled /* && inScope */ => {
                val ee = x.value.asInstanceOf[EError]
                ee.handled = true
                val newD = DomAst(new EInfo("caught:" + ee.msg, ""), AstKinds.GENERATED)
                evAppChildren(a, newD)
                handledError = true

                val p = P.fromSmartTypedValue("exception", Map(
                  "code" -> ee.code,
                  "message" -> ee.msg,
                  "details" -> ee.details
                ))

                exc.append(p)

                val v = DomAst(new EVal(p), AstKinds.GENERATED)
                evAppChildren(a, v)
                setSmartValueInContext(a, ctx.getScopeCtx, p)
              }
            })

            if (ast eq a) {
              done = true // found myself - stop - good for seq todo handle par
            }
          }
        }

        val p = P.fromSmartTypedValue("exceptions", exc.toList)
        val v = DomAst(new EVal(p), AstKinds.GENERATED)
        evAppChildren(a, v)
        setSmartValueInContext(a, ctx.getScopeCtx, p)


        // todo if DONE vs CAUGHT - done means just exception found, no "throw" found

        // IF diesel.throw was used, it will have this attribute in case something was "caught"
        val caught = in.attrs.exists(p => p.name == "caught" && p.calculatedTypedValue.asBoolean == true)

        // skip other children if no exception
        if (!(handledError || caught)) childrenIfAny.foreach { ast =>
          if (!DomState.isDone(ast.status)) {
            ast.status = DomState.SKIPPED
            // todo this bombs if the children are not in tree yet...
//          evChangeStatus(ast, DomState.SKIPPED)
          }
        }

        true

      } else if (ea == DieselMsg.ENGINE.DIESEL_ASSERT) {

        // evaluate all boolean parms
        val cp = in.attrs.map(_.calculatedP)
        val bcp = cp.filter(_.isOfType(WTypes.wt.BOOLEAN)).map { p =>
          p.value.get.asBoolean
        }
        val res = bcp.foldRight(true)((a, b) => a && b)

        val newD =
          if (res && bcp.size > 0)
            DomAst(new EInfo("assert OK"), AstKinds.GENERATED)
          else
            DomAst(new EMsg("diesel", "return",
              P.fromSmartTypedValue(DieselMsg.HTTP.RESPONSE, "Assert failed!") ::
                  P.fromSmartTypedValue(DieselMsg.HTTP.STATUS, 500) ::
                  cp.filter(!_.isOfType(WTypes.wt.BOOLEAN))
            ),
              AstKinds.GENERATED)
        addChild(a, newD)
        true

      } else if (ea == DieselMsg.ENGINE.DIESEL_LATER) {
//      // send new message async later at the root level
//      val EMsg.REGEX(e, m) = ctx.getRequired("msg")
//      val nat = in.attrs.filter(e => !Array("msg").contains(e.name))
//
//      evAppChildren(findParent(findParent(a).get).get, DomAst(EMsg(e, m, nat), AstKinds.GENERATED))
        false

      } else if (ea == DieselMsg.ENGINE.DIESEL_LEVELS) {
        engine.maxLevels = ctx.getRequired("max").toInt
        true

      } else if (ea == DieselMsg.ENGINE.DIESEL_PONG) {

        // expand vals
        val nctx = mkMsgContext(Some(in), calcMsg(a, in)(ctx).attrs, ctx, a)

        val parentId = nctx.getRequired("parentId")
        val targetId = nctx.getRequired("targetId")
        val level = nctx.getRequired("level").toInt

        notifyParent(a, parentId, targetId, level)
        true

      } else if (ea == DieselMsg.ENGINE.DIESEL_VALS) {

        // expand all spec vals
        dom.moreElements.collect {
          case v: EVal => {
            // clone so nobody changes the calculated value of p
            val newv = v.copy(p = v.p.copy().copyFrom(v.p)).copyFrom(v)
            evAppChildren(a, DomAst(newv, AstKinds.TRACE))
            // do not calculate here - keep them more like exprs .calculatedP)

            // todo this causes all kinds of weird issues

//          setSmartValueInContext(a, this.ctx, newv.p)

//          this.engine.ctx.put(newv.p) // do not calculate p, just set it at root level
            a.getCtx.get.getScopeCtx.put(newv.p) // do not calculate p, just set it at root level
          }
        }

        true

      } else if (ea == DieselMsg.ENGINE.DIESEL_WARNINGS) {

        // expand all domain warnings
        dom.moreElements.collect {
          case v: EWarning => {
            evAppChildren(a, DomAst(v, AstKinds.DEBUG))
          }
        }

        true

      } else if (ea == DieselMsg.ENGINE.DIESEL_SYNC) {

        // turn the engine sync
        ctx.root.engine.map(_.synchronous = true)
        true

      } else if (ea == DieselMsg.ENGINE.DIESEL_ASYNC) {

        // turn the engine async
        ctx.root.engine.map(_.synchronous = false)
        true

      } else if (ea == DieselMsg.SCOPE.DIESEL_PUSH) {

        // todo use the DomAst ctx now, not the current
//      this.ctx = new ScopeECtx(Nil, Some(this.ctx), Some(a))
        true

      } else if (ea == DieselMsg.SCOPE.DIESEL_POP) {

        // todo use the DomAst ctx now, not the current
        // find the closest scope and pop it
//      var sc: Option[ECtx] = Some(this.ctx)
//
//      while (sc.isDefined && !sc.exists(p => p.isInstanceOf[ScopeECtx])) {
//        sc = sc.get.base
//      }
//
//      if (sc.isDefined) {
//        this.ctx = this.ctx.base.get
//      } else {
//        evAppChildren(a, DomAst(EError("trying to pop a non-scope...", in.toString)))
//      }

        true

      } else if (ea == DieselMsg.SCOPE.RULE_PUSH) {

        this.ctx = new RuleScopeECtx(Nil, Some(this.ctx), Some(a))
        true

      } else if (ea == DieselMsg.SCOPE.RULE_POP) {

        assert(this.ctx.isInstanceOf[RuleScopeECtx])
        this.ctx = this.ctx.base.get
        true

      } else if (ea == "diesel.engine.debug") {

        val s = this.settings.toJson
        val c = this.ctx.toString
        val e = this.toString
        evAppChildren(a, DomAst(EInfo("settings", js.tojsons(s)), AstKinds.DEBUG))
        evAppChildren(a, DomAst(EInfo("ctx", c), AstKinds.DEBUG))
        evAppChildren(a, DomAst(EInfo("engine", e), AstKinds.DEBUG))
        true

      } else if (ea == DieselMsg.ENGINE.DIESEL_ENG_SET) {

        // todo move this to an executor

        // NOTE: this may be a security risk - they can set trust and stuff -  limit to only some parms
        in.attrs.foreach { p =>
          val v = p.currentStringValue
          p.name match {

            case "diesel.engine.settings.collectCount" | "collectCount" => {
              this.settings.collectCount = Option(v.toInt)
              // if it has collect settings there must be a group
              if (settings.collectGroup.isEmpty) {
                settings.collectGroup = Some(
                  description) // set it to desc just to be sure - it should be reset here as well
              }
            }

            case "diesel.engine.settings.collectGroup" | "collectGroup" => {
              this.settings.collectGroup = Option(v)
            }

            case "diesel.engine.maxLevels" | "maxLevels" => {
              engine.maxLevels = v.toInt
            }

            case "diesel.engine.maxExpands" | "maxExpands" => {
              engine.maxExpands = v.toInt
            }
          }
        }

        evAppChildren(a,
          DomAst(EInfo(s"collect settings updated ${settings.collectCount} - ${settings.collectGroup}"),
            AstKinds.TRACE))

        true

      } else if (ea == "diesel.branch") { // nothing
        true

      } else if (ea == DieselMsg.REALM.REALM_SET) {

        val props = in.attrs
            .filter(_.name == "props")
            .filter(_.isOfType(WTypes.wt.JSON))
            .flatMap(_.calculatedTypedValue.asJson.toList)
            .map(t => P.fromSmartTypedValue(t._1, t._2))

        // todo move this to an executor
        // todo this may be a security risk - they can set trust and stuff -  limit to only some parms?
        (props ::: in.attrs.filter(_.name != "props")).foreach { p =>
          val r = this.settings.realm
          if (r.isEmpty) evAppChildren(a, DomAst(EError("realm not defined...???"), AstKinds.ERROR))
          else {
            r.foreach(Website.putRealmProps(_, p.name, p.calculatedP))
            r.flatMap(Website.forRealm).map(_.put(p.name, p.calculatedValue))
            evAppChildren(a, DomAst(EInfo("updated..."), AstKinds.DEBUG))
          }
        }
        true

      } else if (ea == DieselMsg.ENGINE.DIESEL_MAP) {

        // expanding diesel.map: make an item for each item and copy all children under the item

        // TODO problem is the indented children are evaludated and added AFTER this :(

        // save children and delete
        val parent = findParent(a).flatMap(findParent)
        val children = a.children
        a.childrenCol.clear()

        var info: List[Any] = Nil

        info = EInfo(s"engine msg") :: info

        // l can be a constant with another parm name OR the actual array
        val list = {
          val l = ctx.getRequiredp(Diesel.PAYLOAD).calculatedP
          if (l.isOfType(WTypes.wt.ARRAY)) {
            l
          } else if (l.isOfType(WTypes.wt.STRING)) {
            ctx.getRequiredp(l.currentStringValue)
          } else {
            info = EWarning(s"Can't source input list - what type is it? ${l}") :: info
            P("", "", WTypes.wt.UNDEFINED) //throw new IllegalArgumentException(s"Can't source input list: $ctxList")
          }
        }

        razie.js.parse(s"{ list : ${list.currentStringValue} }").apply("list") match {

          case l: collection.Seq[Any] => {
            // passing any other parameters that were given to foreach

            l.map { item: Any =>
              // for each item in list, create message
              val itemP = P.fromTypedValue(Diesel.PAYLOAD, item)
              val m = EMsg("diesel", "mapItem", itemP :: Nil)
              val ast = DomAst(m, AstKinds.GENERATED)
              evAppChildren(a, ast)

              // copy children under each item
              // TODO problem is the indented children are evaludated and added AFTER this :(
//            children.foreach { c =>
//              evAppChildren(ast, DomAst(c.value, AstKinds.GENERATED))
//            }
            }.toList
          }

          case x@_ => {
            List(EError("value to iterate on was not a list", x.getClass.getName) :: info)
          }

        }

        info.map { x =>
          evAppChildren(a, DomAst(x, AstKinds.TRACE))
        }

        // expand all spec vals
        dom.moreElements.collect {
          case v: EVal => {
            evAppChildren(a, DomAst(v, AstKinds.TRACE))
            setSmartValueInContext(a, this.ctx, v.p)
          }
        }

        true

      } else {

        false
      }
    }

    (res, skippedNodes.toList, newNodes.toList)
  }

  // if the message attrs were expressions, calculate their values
  private def calcMsg(a: DomAst, in: EMsg)(implicit ctx: ECtx): EMsg = {
    val calcAttrs = in.attrs.flatMap { p =>
      // this flattening is a duplicate of EMap.sourceAttrs but required for $send which is not calculated...
      // flattening objects first
      if (p.expr.exists { e =>
        e.isInstanceOf[AExprIdent] &&
            e.asInstanceOf[AExprIdent].rest.size > 0 &&
            e.asInstanceOf[AExprIdent].rest.last.name.equals("asAttrs")
      }) {
        p :: flattenJson(p)
      } else if (p.name.endsWith(".asAttrs")) {
        // we have to resolve it here and flatten it
        val ap = new SimpleExprParser().parseIdent(p.name).map(_.dropLast)
        p :: flattenJson(p.copy(expr = ap).calculatedP)
      } else {
        List(p.calculatedP) // only calculate if not already calculated =likely in a different context=
      }
    }

    // add trace info for undefined parms
    calcAttrs
        .filter(_.ttype == WTypes.UNDEFINED)
        .foreach(attr => {
          evAppChildren(a, DomAst(EInfo(s"""UNDEFINED parm $attr"""), AstKinds.DEBUG))
        })

    in.copy(
      attrs = calcAttrs.filter(_.ttype != WTypes.UNDEFINED) // an undefined parm is the same as not passed in !!
    ).copiedFrom(in)
  }

  /** expand a single message and rep - called by engine, results in engine processing */
  private def expandEMsgAndRep(a: DomAst, in: EMsg, recurse: Boolean, level: Int, parentCtx:ECtx) : List[DEMsg] = {
    val (newNodes, skippedNodes) = expandEMsg(a, in, recurse, level, parentCtx)

    // analyze the new messages and return

    // this is what makes this synchronous behaviour - it reps itself as opposed to waiting for an async DEReply
    rep(a, recurse, level, newNodes) //::: skippedNodes.flatMap(freeDepys)
  }

  /** test epected message */
  private def expandExpectM(a: DomAst, e: ExpectM) : Unit = {
    val cole = new MatchCollector()

    implicit val ctx = a.getCtx.get

    Try {

      var targets = e.target.map(List(_)).getOrElse(if (e.when.isDefined) {
        // find generated messages that should be tested
        root.collect {
          case d@DomAst(n: EMsg, k, _, _) if e.when.exists(_.test(a, n)) => d
        }
      } else List(root))

      if (targets.size > 0) {
        // todo look at all possible targets - will need to create a matchCollector per etc
        targets.head.collect {
          case d@DomAst(n: EMsg, k, _, _) if AstKinds.isGenerated(k) =>
            cole.newMatch(d)
            if (e.m.test(a, n, Some(cole)))
              evAppChildren(a, DomAst(TestResult("ok").withTarget(e), AstKinds.TRACE).withSpec(e))
        }

        cole.done

        // if it matched some Msg then highlight it there
        if (cole.highestMatching.exists(c =>
          c.score >= 2 &&
              c.diffs.values.nonEmpty &&
              c.x.isInstanceOf[DomAst] &&
              c.x.asInstanceOf[DomAst].value.isInstanceOf[EMsg]
        )) {
          val c = cole.highestMatching.get
          val d = c.x.asInstanceOf[DomAst]
          val m = d.value.asInstanceOf[EMsg]
          val s = c.diffs.values.map(_._2).toList.map(
            x => s"""<span style="color:red">${htmlValue(x.toString)}</span>""").mkString(",")
          d.moreDetails = d.moreDetails + label("expected", "danger") + " " + s
        }

        // did some rules succeed?
        if (a.children.isEmpty) {
          // oops - add test failure
          evAppChildren(a, DomAst(
            TestResult(
              "fail",
              "",
              label("found", "warning") + " " +
                  cole.highestMatching.map(
                    _.diffs.values.map(_._1).toList.map(x => s"""<span style="color:red">$x</span>""").mkString(
                      ",")).mkString
            ).withTarget(e),
            AstKinds.TEST
          ).withSpec(e))
        }
      }

  }.recover {
    case t:Throwable => {

      razie.Log.error("while decompose em()", t)

      // oops - add test failure
      evAppChildren(a, DomAst(
        TestResult(
          "fail",
          "",
          cole.toHtml
        ).withTarget(e),
        AstKinds.TEST
      ).withSpec(e))

      evAppChildren(a, DomAst(new EError("Exception", t), AstKinds.ERROR).withStatus(DomState.DONE))
    }
  }.get
  }

  /** test epected values */
  private def expandExpectV(a: DomAst, e: ExpectV) : Unit = {
    val cole = new MatchCollector()

    Try {

    // identify sub-trees that it applies to

    val subtrees = e.target.map(List(_)).getOrElse(if (e.when.isDefined) {
      // find generated messages that should be tested
      root.collect {
        case d@DomAst(n: EMsg, k, _, _) if e.when.exists(_.test(a, n)) => d
      }
    } else List(root))

    if (subtrees.size > 0) {
      // todo look at all possible targets - will need to create a matchCollector per etc

      subtrees.foreach { aTarget =>

        // collecting all Values created in the sub-tree
        var vvals = aTarget.collect {
          case d@DomAst(n: EVal, k, _, _) if AstKinds.isGenerated(k) => d
        }

        // reverse (so last overwrites first) and distinct (so multiple payloads don't act funny)
        vvals = vvals.reverse.groupBy(_.value.asInstanceOf[EVal].p.name).values.toList.map(_.head)
        val values = vvals.map(_.value.asInstanceOf[EVal].p)

        // bug: if the target had (x=y) and produced value x then newctx would overwrite the newer x, so
        // we're overwriting it again here by putting values in front of attrs

        // new evaluation context - include the message's values in its context
        if (!aTarget.value.isInstanceOf[EMsg]) {
          throw new DieselExprException("$expect must follow a $send...")
        }

        // we base off the parent scope (story scope context)
        // todo should we base off the target scope and include the target's internal parameters?
//        val newctx = new StaticECtx(
//          values ::: aTarget.value.asInstanceOf[EMsg].attrs.filter(b => !values.exists(a => a.name == b.name)),
//          values ::: aTarget.value.asInstanceOf[EMsg].attrs,
//          aTarget.getCtx,
//          Some(aTarget))

        // include target's attrs but not to overwrite context
//        val newctx = new StaticECtx(
//          values ::: aTarget.value.asInstanceOf[EMsg].attrs.filter(b => !values.exists(a => a.name == b.name)),
//          aTarget.getCtx,
//          Some(aTarget))

        // include target's attrs but not to overwrite context
        val newctx = new StaticECtx(
          values ::: aTarget.value.asInstanceOf[EMsg].attrs.filter(b => !values.exists(a => a.name == b.name)),
          Some(a.getCtx.get.getScopeCtx),
//          a.getCtx,
          Some(aTarget))


        // start checking now

        if (!aTarget.value.isInstanceOf[EMsg]) {
          // wtf did we just target?
          a append DomAst(
            TestResult("fail", "Target not a message - did something run?").withTarget(e),
            AstKinds.TEST
          ).withSpec(e)
        } else if (vvals.size > 0 && !e.applicable(a, values)(newctx)) {
          // n/a - had a guard and guard not met
          a append DomAst(
            TestResult("n/a", "$if condition not met").withTarget(e),
            AstKinds.TEST
          ).withSpec(e)
        } else if (vvals.size > 0 && e.test(a, values, Some(cole), vvals)(newctx)) {
          // test ok
          a append DomAst(
            TestResult("ok").withTarget(e),
            AstKinds.TRACE
          ).withSpec(e)
        } else if (vvals.size == 0 && !e.applicable(a, values)(newctx)) {
          // targeted tree generated no values, so this is a global state condition
          // n/a - had a guard and guard not met
          a append DomAst(
            TestResult("n/a", "$if condition not met").withTarget(e),
            AstKinds.TEST
          ).withSpec(e)
        } else if (vvals.size == 0 && e.test(a, Nil, Some(cole), vvals)(newctx)) {
          // targeted tree generated no values, so this is a global state condition

          a append DomAst(
            TestResult("ok").withTarget(e),
            AstKinds.TRACE
          ).withSpec(e)
        } else
        //if no rules succeeded and there were vals, collect the misses
          values.map(v => cole.missed(v.name))
      }

      cole.done

      // did some rules succeed?
      if (a.children.isEmpty) {
        // oops - add test failure
        a append DomAst(
          TestResult(
            "fail",
            "",
            cole.toHtml
          ).withTarget(e),
          AstKinds.TEST
        ).withSpec(e)
      }

      // if it matched some name then highlight it there
      // any match means a name matched ?
      if (cole.highestMatching.exists(c =>
        c.score >= 0 &&
          c.diffs.values.nonEmpty &&
          c.x.isInstanceOf[DomAst] &&
          c.x.asInstanceOf[DomAst].value.isInstanceOf[EVal]
      )) {
        val c = cole.highestMatching.get
        val d = c.x.asInstanceOf[DomAst]
        val m = d.value.asInstanceOf[EVal]
        val s = c.diffs.values.map(_._2).toList.map(x => s"""<span style="color:red">${htmlValue(x.toString)}</span>""").mkString(",")
        d.moreDetails = d.moreDetails + label("expected", "danger") + " " + s // don't do htmlValue(s) - it has purposeful <span> etc
      }

      // did some rules succeed?
      if (a.children.isEmpty) {
        // oops - add test failure
        a append DomAst(
          TestResult(
            "fail",
            "",
            label("found", "warning") + " " +
                cole.highestMatching.map(_.diffs.values.map(_._1).toList.map(
                  x => s"""<span style="color:red">${htmlValue(x.toString)}</span>""").mkString(",")).mkString
          ).withTarget(e),
          AstKinds.TEST
        ).withSpec(e)
      }
    }

    }.recover {
      case t:Throwable => {

        razie.Log.log(s"while decompose ExpectV: $e", t)

        // oops - add test failure
        a append DomAst(
          TestResult(
            "fail",
            "Exception: " + t.getClass.getSimpleName + ": " + t.getMessage,
            cole.toHtml
          ).withTarget(e),
          AstKinds.TEST
        ).withSpec(e)

        a append DomAst(new EError("Exception", t), AstKinds.ERROR).withStatus(DomState.DONE)
      }
    }.get
  }

  private def expandExpectAssert(a: DomAst, e: ExpectAssert) : Unit = {
    val cole = new MatchCollector()

    // identify sub-trees that it applies to

    val subtrees = e.target.map(List(_)).getOrElse(if (e.when.isDefined) {
      // find generated messages that should be tested
      root.collect {
        case d@DomAst(n: EMsg, k, _, _) if e.when.exists(_.test(a, n)) => d
      }
    } else List(root))

    if (subtrees.size > 0) {
      // todo look at all possible targets - will need to create a matchCollector per etc

      subtrees.foreach {n=>
        val vvals = n.collect {
          case d@DomAst(n: EVal, k, _, _) if AstKinds.isGenerated(k) => d
        }

        // include the message's values in its context
        val newctx = new StaticECtx(n.value.asInstanceOf[EMsg].attrs, Some(ctx), Some(n))

        val values = vvals.map(_.value.asInstanceOf[EVal].p)

        if (vvals.size > 0 && e.test(a, values, Some(cole), vvals)(newctx))
          a append DomAst(
            TestResult("ok").withTarget(e),
            AstKinds.TRACE
          ).withSpec(e)
        else
        //if no rules succeeded and there were vals, collect the misses
          values.map(v => cole.missed(v.name))
    }

      cole.done

      // did some rules succeed?
      if (a.children.isEmpty) {
        // oops - add test failure
        a append DomAst(
          TestResult(
            "fail",
            cole.toHtml
          ).withTarget(e),
          AstKinds.TEST
        ).withSpec(e)
      }

      // did some rules succeed?
      if (a.children.isEmpty) {
        // oops - add test failure
        a.append(DomAst(
          TestResult(
            "fail",
            label("found", "warning") + " " +
                cole.highestMatching.map(_.diffs.values.map(_._1).toList.map(
                  x => s"""<span style="color:red">${htmlValue(x.toString)}</span>""").mkString(",")).mkString
          ).withTarget(e),
          AstKinds.TEST
        ).withSpec(e))
      }
    }
  }

}


