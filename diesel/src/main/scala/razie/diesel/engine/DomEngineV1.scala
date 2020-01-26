/*  ____    __    ____  ____  ____  ___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

import org.bson.types.ObjectId
import razie.diesel.Diesel
import razie.diesel.dom.RDOM.P
import razie.diesel.dom.{RDomain, WTypes}
import razie.diesel.engine.RDExt._
import razie.diesel.engine.exec.{EApplicable, Executors}
import razie.diesel.engine.nodes._
import razie.diesel.expr._
import razie.diesel.model.DieselMsg
import razie.js
import razie.tconf.DSpec
import scala.Option.option2Iterable
import scala.collection.mutable.HashMap
import scala.collection.parallel.mutable
import scala.util.Try
import DieselMsg.ENGINE._
import razie.hosting.Website

/** the actual engine implementation
  *
  * we expand and execute the known nodes
  */
class DomEngineV1(
  dom: RDomain,
  root: DomAst,
  settings: DomEngineSettings,
  pages : List[DSpec],
  description:String,
  id:String = new ObjectId().toString)
    extends DomEngine (
  dom, root, settings, pages, description, id
    ) {

  assert(settings.realm.isDefined, "need realm defined for engine settings")

  /** transform one element / one step
    *
    * 1. you can collect children of the current node, like info nodes etc
    * 2. return continuations, including processing through the children
    *
    * @param a node to decompose
    * @param recurse should recurse
    * @param level
    * @return continuations
    */
  protected override def expand(a: DomAst, recurse: Boolean = true, level: Int): List[DEMsg] = {
    var msgs : List[DEMsg] = Nil // continuations from this cycle

    if (level >= maxLevels) {
      evAppChildren(a, DomAst(TestResult("fail: Max-Recurse!", "You have a recursive rule generating this branch..."), "error"))
      return Nil
    }

    if(this.curExpands > maxExpands) {
      // THIS IS OK: append direct to children
      a append DomAst(TestResult("fail: Max-Expand!", s"You have expanded too many nodes (>$maxExpands)..."), "error")
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

      case stop : EEngStop => {
        // stop engine
        evAppChildren(a, DomAst(EInfo(s"""Stopping engine <a href="/diesel/engine/view/${this.id}">${this.id}</a>""")))//.withPos((m.get.pos)))
        stopNow
      }

      case stop : EEngSuspend => {
        // suspend and wait for a continuation async
        evAppChildren(a, DomAst(EInfo(s"""Suspending engine <a href="/diesel/engine/view/${this.id}">${this.id}</a>""")))//.withPos((m.get.pos)))
        stop.onSuspend.foreach(_.apply(this, a, level))
      }

      case next@ENext(m, "==>", cond, _, _) => {
        // forking new engine / async branch
        implicit val ctx = new StaticECtx(next.parent.map(_.attrs).getOrElse(Nil), Some(this.ctx), Some(a))
        // start new engine/process
        // todo should find settings for target service  ?
        val eng = spawn(List(DomAst(next.evaluateMsg)))
        eng.process // start it up in the background
        evAppChildren(a, DomAst(EInfo(s"""Spawn engine <a href="/diesel/engine/view/${eng.id}">${eng.id}</a>""")))//.withPos((m.get.pos)))
        // no need to return anything - children have been decomposed
      }

      case n1@ENext(m, ar, cond, _, _) if "-" == ar || "=>" == ar => {
        // message executed later
        // todo bubu: static parent parms overwrite side effects updated in this.ctx
        implicit val ctx = new StaticECtx(n1.parent.map(_.attrs).getOrElse(Nil), Some(this.ctx), Some(a))

        if(n1.test()) {
          val newmsg = n1.evaluateMsg
          val newnode = DomAst(newmsg, AstKinds.kindOf(newmsg.arch))

          // old node may have had children already
          newnode.childrenCol.appendAll(a.children)
          a.childrenCol.clear() // need to remove them from parent, or they will be duplos and create problems

          msgs = rep(a, recurse, level, List(newnode))
        }
        // message will be evaluate() later
      }

      case n1@ENextPas(m, ar, cond, _, _) if "-" == ar || "=>" == ar => {
        implicit val ctx = mkMsgContext(
          n1.parent,
          n1.parent.map(_.attrs).getOrElse(Nil),
          this.ctx,
          a)

        if(n1.test()) {
          appendValsPas (a, m, m.attrs, ctx)
        }
      }

      case n1@EMsgPas(ar) => {
        implicit val ctx = mkMsgContext(
          None,
          Nil,
          this.ctx,
          a)

        appendValsPas (a, n1, n1.attrs, ctx)
      }

      case x: EMsg if x.entity == "" && x.met == "" => {
        // just expressions, generate values from each attribute
        appendVals (a, x, x.attrs, ctx, AstKinds.kindOf(x.arch))
      }

      case in: EMsg => {
        // todo should collect all parent contexts from root to here...
        // todo problem right now: parent parms are not in context, so can't use in child messages
//        implicit val parentCtx = new StaticECtx(in.attrs, Some(this.ctx), Some(a))
        msgs = expandEMsgX(a, in, recurse, level, this.ctx) ::: msgs
      }

      case n: EVal if !AstKinds.isGenerated(a.kind) => {
        // $val defined in scope
        val p = n.p.calculatedP // only calculate if not already calculated =likely in a different context=
        a append DomAst(EVal(p).withPos(n.pos), AstKinds.TRACE)
        ctx.put(p)
      }

      case e: ExpectM => if(!settings.simMode) expandExpectM(a, e)

      case e: ExpectV => if(!settings.simMode) expandExpectV(a, e)

      case e: ExpectAssert => if(!settings.simMode) expandExpectAssert(a, e)

      case e:InfoNode =>  // ignore
      case e:EVal =>  // ignore

      // todo should execute
      case s: EApplicable =>  {
        clog << "EXEC KNOWN: " + s.toString
        evAppChildren(a, DomAst(EError("Can't EApplicable KNOWN - " + s.getClass.getSimpleName, s.toString), AstKinds.ERROR))
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

  /** make a static context for children */
  protected def mkMsgContext (in:Option[EMsg], attrs:List[P], parentCtx:ECtx, a:DomAst) = {
    new StaticECtx(
      in.map(in=>P(DIESEL_MSG_ENTITY, in.entity)).toList :::
      in.map(in=>P(DIESEL_MSG_ACTION, in.met)).toList :::
      in.map(in=>P(DIESEL_MSG_EA, in.ea)).toList :::
          // todo avoid this every time - lazy parms ?
      List(P.fromTypedValue(DIESEL_MSG_ATTRS, attrs.map(p=> (p.name, p.calculatedTypedValue(parentCtx).value)).toMap)) :::
      attrs,
      Some(parentCtx),
      Some(a))
  }

  /** expand a single message */
  protected override def expandEMsg(a: DomAst, in: EMsg, recurse: Boolean, level: Int, parentCtx:ECtx) : List[DomAst] = {
    var newNodes : List[DomAst] = Nil // nodes generated this call collect here

    // implicit var ctx = new StaticECtx(in.attrs, Some(parentCtx), Some(a))
    implicit var ctx = parentCtx

    // expand message expr if the message attrs were expressions, calculate their values
    val n: EMsg = calcMsg(in)(ctx)
    // replace it so we see the calculated values
    a.value = n

    // this is causing BIG WEIRD problems...
//     a.children.append(DomAst(in, AstKinds.TRACE).withStatus(DomState.SKIPPED))
//    a.children.append(DomAst(EInfoWrapper(in), AstKinds.TRACE).withStatus(DomState.SKIPPED))

    ctx = new StaticECtx(n.attrs, Some(parentCtx), Some(a))

    // PRE - did it already have some children, decomposed by someone else?
    newNodes = a.children
    a.childrenCol.clear() // remove them so we don't have duplicates in tree - they will be processed later

    // 1. engine message?
    var mocked = expandEngineEMsg(a, n)

    var mocksApplied = HashMap[String, EMock]()

    // 2. if not, look for mocks
    if (settings.mockMode) {
      val exclusives = HashMap[String, ERule]()
      var matchingRules = (root.collect {
        // mocks from story AST
        case d@DomAst(m: EMock, _, _, _) if m.rule.e.test(n) && a.children.isEmpty => m
      } ::: dom.moreElements.toList).collect {
        // todo perf optimize moreelements.toList above
        // plus mocks from spec dom
        case m: EMock if m.rule.e.test(n) && a.children.isEmpty => m
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
      var matchingRules = rules.filter(_.e.test(n))
      val exclusives = HashMap[String, ERule]()

      // use fallbacks?
      if (matchingRules.isEmpty)
        matchingRules =
            rules
                .filter(_.arch contains "fallback")
                .filter(_.e.test(n, None, true))

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
          if(r.arch.contains("exclusive")) {
            exclusives.put (r.e.cls + "." + r.e.met, r) // we do exclusive per pattern
          }

          newNodes = newNodes ::: ruleDecomp(a, n, r, ctx)
        }
      }
    }

    // any generated sub-messages go in sequence - important as many mocks and rules may apply
    // side-effecting
    if(newNodes.size > 1) newNodes.drop(1).foldLeft(newNodes.head)((a,b)=> {
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
        (true /*!settings.mockMode || x.isMock*/) && x._2.test(n)
      }.map { t =>
        val r = t._2
        mocked = true

        val news = try {
          // need to use ctx not this.ctx, to respect scopes etc
//          implicit val ctx = new StaticECtx(n.attrs, Some(this.ctx), Some(a))

          val xx = r.apply(n, None)

          // step 1: expand lists
          val yy = xx.flatMap(x=> {
            x match {
              case l:List[_] => l
              case e@_ => List(e)
            }
          })

          yy.collect{
            // collect resulting values in the context as well
            case v@EVal(p) => {
              ctx.put(p)
              v
            }
            case p:P => {
              val v = EVal(p)
              ctx.put(p)
              v
            }
//            case l@List => e
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
                        else if (x.isInstanceOf[EDuration]) AstKinds.DEBUG
                        else if (x.isInstanceOf[EVal]) x.asInstanceOf[EVal].kind.getOrElse(AstKinds.GENERATED)
                        else if (x.isInstanceOf[EError]) AstKinds.ERROR
                        else AstKinds.GENERATED
                        )
                  )
                ).withSpec(r)
          }
        } catch {
          case e: Throwable =>
            razie.Log.alarmThis("wtf", e)
            val p = EVal(P(Diesel.PAYLOAD, e.getMessage, WTypes.wt.EXCEPTION).withValue(e, WTypes.wt.EXCEPTION))
            ctx.put(p.p)
            List(DomAst(new EError("Exception:", e), AstKinds.ERROR), DomAst(p, AstKinds.ERROR))
        }

        newNodes = newNodes ::: news
      })

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
          evAppChildren(a, DomAst(
            EWarning(
              "No rules, mocks or executors match for " + in.toString,
              s"Review your engine configuration (blender=${settings.blenderMode}, mocks=${settings.blenderMode}, drafts=${settings.blenderMode}, tags), " +
                  "spelling of messages or rule clauses / pattern matches"),
            AstKinds.DEBUG
          ))
        }
      }
    }

    // 4. sketch

    // last ditch attempt, in sketch mode: if no mocks or rules, run the expects
    if (!mocked && settings.sketchMode) {
      // sketch messages - from AST and then dom, as the stories are not always told
      // when running as an API from REST (wrest) the story is collected in DOM not in AST
      (collectValues {
        case x: ExpectM if x.when.exists(_.test(n)) => x
      }.headOption
          orElse
          dom.moreElements.collect {
            case x: ExpectM if x.when.exists(_.test(n)) => x
          }.headOption
          ).map { e =>
        mocked = true

        val spec = dom.moreElements.collect {
          case x: EMsg if x.entity == e.m.cls && x.met == e.m.met => x
        }.headOption

        val newctx = new StaticECtx(n.attrs, Some(ctx), Some(a))
        val news = e.sketch(None)(newctx).map(x => DomAst(x, AstKinds.SKETCHED).withSpec(e))
        newNodes = newNodes ::: news
      }

      // sketch messages - from AST and then dom, as the stories are not always told
      // when running as an API from REST (wrest) the story is collected in DOM not in AST
      (collectValues {
        case x: ExpectV if x.when.exists(_.test(n)) => x
      }.headOption
          orElse
          dom.moreElements.collect {
            case x: ExpectV if x.when.exists(_.test(n)) => x
          }.headOption
          ).map { e =>
        val newctx = new StaticECtx(n.attrs, Some(ctx), Some(a))
        val news = e.sketch(None)(newctx).map(x => EVal(x)).map(x => DomAst(x, AstKinds.SKETCHED).withSpec(e))
        mocked = true

        news.foreach { n =>
          evAppChildren(a, n)
        }
      }
    }

    newNodes
  }

  /** reused - execute a rule */
  private def ruleDecomp (a:DomAst, in:EMsg, r:ERule, parentCtx:ECtx) = {
    var result : List[DomAst] = Nil
    implicit val ctx : ECtx = new StaticECtx(in.attrs, Some(parentCtx), Some(a))

    val parents = new collection.mutable.HashMap[Int, DomAst]()

    def findParent(level:Int) : DomAst = {
      if(level >= 0) parents.get(level).getOrElse(findParent(level - 1))
      else throw new DieselExprException("can't find parent in ruleDecomp")
    }

    // used to build trees from flat list with levels
    def addChild(x:Any, level:Int) = {
      val ast = DomAst(x, AstKinds.NEXT).withSpec(r)
      parents.put(level, ast)
      if(level == 0) Some(ast)
      else if(level > 0) {
        val parent = findParent(level - 1)
        parent.childrenCol.append(ast) // todo I'm not creating events for persist for this
        None//child was consumed
      } else {
        throw new DieselExprException("child level negative in ruleDecomp")
      }
    }

    // generate each gen/map
    r.i.map { ri =>
      // find the spec of the generated message, to ref
      val spec = dom.moreElements.collect {
        case x: EMsg if x.entity == ri.cls && x.met == ri.met => x
      }.headOption
      val KIND = AstKinds.kindOf(r.arch)

      val generated = ri.apply(in, spec, r.pos, r.i.size > 1, r.arch)

      // defer evaluation if multiple messages generated - if just one message, evaluate values now
      var newMsgs = generated.collect {
        // hack: if only values then collect resulting values and dump message
        // only for size 1 because otherwise they may be in a sequence of messages and their value
        // may depend on previous messages and can't be evaluated now
        //
        case x: EMsg if x.entity == "" && x.met == "" && r.i.size == 1 => {
          // these go into the ctx as well
          appendVals(a, x, x.attrs, ctx, KIND)
          None
        }

        case x: EMsg if x.entity == "" && x.met == "" && r.i.size > 1 => {
          // can't execute now, but later
          Some(DomAst(ENext(x, "=>", None, true), AstKinds.NEXT).withSpec(r))
        }

        // else collect message
        case x: EMsg => {
          if(r.i.size > 1)
            Some(DomAst(ENext(x, "=>"), AstKinds.NEXT).withSpec(r))
          else
            Some(DomAst(x, KIND).withSpec(r))
        }

        case x: ENext => {
          addChild(x, x.indentLevel)
        }

        case x: ENextPas => {
          addChild(x, x.indentLevel)
        }

        case x @ _ => {
          // vals and whatever other things
          Some(DomAst(x, KIND).withSpec(r))
        }
      }.filter(_.isDefined)

      // if multiple results from a single map, default to sequence, i.e. make them dependent
      //        newMsgs.drop(1).foldLeft(newMsgs.headOption.flatMap(identity))((a,b)=> {
      //          Some(b.get.withPrereq(List(a.get.id)))
      //        })

      result = result ::: newMsgs.flatMap(_.toList)
    }
    // if multiple results from a single map, default to sequence, i.e. make them dependent
    if(result.size > 1) result.drop(1).foldLeft(result.head)((a,b)=> {
      b.withPrereq(List(a.id))
    })

    result
  }

  /** if it's an internal engine message, execute it */
  private def expandEngineEMsg(a: DomAst, in: EMsg) : Boolean = {
    val ea = in.ea
    if(ea == DieselMsg.ENGINE.DIESEL_RETURN) {
      // expand all spec vals
      in.attrs.map(_.calculatedP).foreach {p=>
        evAppChildren(a, DomAst(EVal(p)))
        this.ctx.put(p)
      }

      // stop other children
      findParent(a).collect {
        // first parent may be a ENext, see through
        case ast if ast.value.isInstanceOf[ENext] => findParent(ast)
        case ast@_ => Some(ast)
      }.flatten.toList.flatMap(_.children).foreach {ast=>
        if(! DomState.isDone(ast.status)) {
          evChangeStatus(ast, DomState.SKIPPED)
        }
      }
      true
    } else if(ea == DieselMsg.ENGINE.DIESEL_VALS) {
      // expand all spec vals
      dom.moreElements.collect {
        case v:EVal => {
          evAppChildren(a, DomAst(v, AstKinds.TRACE))
          this.ctx.put(v.p)
        }
      }

      true
    } else if(ea == DieselMsg.SCOPE.DIESEL_PUSH) {
      this.ctx = new ScopeECtx(Nil, Some(this.ctx), Some(a))
      true
    } else if(ea == DieselMsg.SCOPE.DIESEL_POP && this.ctx.isInstanceOf[ScopeECtx]) {
      this.ctx = this.ctx.base.get
      true
    } else if(ea == "diesel.engine.debug") {
      val s = this.settings.toJson
      val c = this.ctx.toString
      val e = this.toString
      evAppChildren(a, DomAst(EInfo("settings", js.tojsons(s)), AstKinds.DEBUG))
      evAppChildren(a, DomAst(EInfo("ctx", c), AstKinds.DEBUG))
      evAppChildren(a, DomAst(EInfo("engine", e), AstKinds.DEBUG))
      true
    } else if(ea == DieselMsg.REALM.REALM_SET) {
      // todo move this to an executor
      // todo this may be a security risk - they can set trust and stuff -  limit to only some parms?
      in.attrs.foreach{ p=>
        this.settings.realm.flatMap(Website.forRealm).map(_.put(p.name, p.calculatedValue))
      }
      true
    } else {
      false
    }
  }

  // if the message attrs were expressions, calculate their values
  private def calcMsg (in:EMsg)(implicit ctx: ECtx) : EMsg = {
    in.copy(
      attrs = in.attrs.flatMap { p =>
        // flattening objects first
        if(p.expr.exists{e=>
            e.isInstanceOf[AExprIdent] &&
                e.asInstanceOf[AExprIdent].rest.size > 0 &&
                e.asInstanceOf[AExprIdent].rest.last.name.equals("asAttrs")
        }) {
          p :: flattenJson(p)
        } else if(p.name.endsWith(".asAttrs")) {
          // we have to resolve it here and flatten it
          val ap = new SimpleExprParser().parseIdent(p.name).map(_.dropLast)
          p :: flattenJson(p.copy(expr = ap).calculatedP)
        } else {
          List(p.calculatedP) // only calculate if not already calculated =likely in a different context=
        }
      }.filter (_.ttype != WTypes.UNDEFINED) // an undefined parm is the same as not passed in !!
    ).copiedFrom(in)
  }

  /** expand a single message - called by engine, results in engine processing */
  private def expandEMsgX(a: DomAst, in: EMsg, recurse: Boolean, level: Int, parentCtx:ECtx) : List[DEMsg] = {
    val newNodes = expandEMsg(a, in, recurse, level, parentCtx)

    // analyze the new messages and return

    // this is what makes this synchronous behaviour - it reps itself as opposed to waiting for an async DEReply
    rep(a, recurse, level, newNodes)
  }

  /** test epected message */
  private def expandExpectM(a: DomAst, e: ExpectM) : Unit = {
    val cole = new MatchCollector()

    Try {

    var targets = e.target.map(List(_)).getOrElse(if (e.when.isDefined) {
      // find generated messages that should be tested
      root.collect {
        case d@DomAst(n: EMsg, k, _, _) if e.when.exists(_.test(n)) => d
      }
    } else List(root))

    if (targets.size > 0) {
      // todo look at all possible targets - will need to create a matchCollector per etc
      targets.head.collect {
        case d@DomAst(n: EMsg, k, _, _) if AstKinds.isGenerated(k) =>
          cole.newMatch(d)
          if (e.m.test(n, Some(cole)))
            evAppChildren(a, DomAst(TestResult("ok").withPos(e.pos), "test").withSpec(e))
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
        val s = c.diffs.values.map(_._2).toList.map(x => s"""<span style="color:red">${htmlValue(x.toString)}</span>""").mkString(",")
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
            cole.highestMatching.map(_.diffs.values.map(_._1).toList.map(x => s"""<span style="color:red">$x</span>""").mkString(",")).mkString
          ).withPos(e.pos),
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
        ),
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
        case d@DomAst(n: EMsg, k, _, _) if e.when.exists(_.test(n)) => d
      }
    } else List(root))

    if (subtrees.size > 0) {
      // todo look at all possible targets - will need to create a matchCollector per etc

      subtrees.foreach {n=>
        var vvals = n.collect {
          case d@DomAst(n: EVal, k, _, _) if AstKinds.isGenerated(k) => d
        }

        // reverse (so last overwrites first) and distinct (so multiple payloads don't act funny)
        vvals = vvals.reverse.groupBy(_.value.asInstanceOf[EVal].p.name).values.toList.map(_.head)

        // include the message's values in its context
        def newctx = new StaticECtx(n.value.asInstanceOf[EMsg].attrs, Some(ctx), Some(n))

        val values = vvals.map(_.value.asInstanceOf[EVal].p)

        if (!n.value.isInstanceOf[EMsg]) {
          // wtf did we just target?
          a append DomAst(
            TestResult("fail", "Target not a message - did something run?").withPos(e.pos),
            AstKinds.TEST
          ).withSpec(e)
        } else if (vvals.size > 0 &&
          !e.applicable(values)(newctx)) {
          // n/a
          a append DomAst(
            TestResult("n/a").withPos(e.pos),
            AstKinds.TEST
          ).withSpec(e)
        } else if (vvals.size > 0 &&
          e.test(values, Some(cole), vvals)(newctx)) {
          // test ok
          a append DomAst(
            TestResult("ok").withPos(e.pos),
            AstKinds.TEST
          ).withSpec(e)
        } else if (vvals.size == 0 &&
          e.test(Nil, Some(cole), vvals)(newctx)) {
          // previous generated no values, so this is a global state condition

          a append DomAst(
            TestResult("ok").withPos(e.pos),
            AstKinds.TEST
          ).withSpec(e)
        } else
        //if no rules succeeded and there were vals, collect the misses
          values.map(v=> cole.missed(v.name))
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
          ),
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
            cole.highestMatching.map(_.diffs.values.map(_._1).toList.map(x => s"""<span style="color:red">${htmlValue(x.toString)}</span>""").mkString(",")).mkString
          ).withPos(e.pos),
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
            "Exception: " + t.toString,
            cole.toHtml
          ),
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
        case d@DomAst(n: EMsg, k, _, _) if e.when.exists(_.test(n)) => d
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

        if (vvals.size > 0 && e.test(values, Some(cole), vvals)(newctx))
          a append DomAst(
            TestResult("ok").withPos(e.pos),
            AstKinds.TEST
          ).withSpec(e)
        else
          //if no rules succeeded and there were vals, collect the misses
          values.map(v=> cole.missed(v.name))
    }

      cole.done

      // did some rules succeed?
      if (a.children.isEmpty) {
        // oops - add test failure
        a append DomAst(
          TestResult(
            "fail",
            cole.toHtml
          ),
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
              cole.highestMatching.map(_.diffs.values.map(_._1).toList.map(x => s"""<span style="color:red">${htmlValue(x.toString)}</span>""").mkString(",")).mkString
          ).withPos(e.pos),
          AstKinds.TEST
        ).withSpec(e))
      }
    }
  }

}


