/*  ____    __    ____  ____  ____  ___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

import org.bson.types.ObjectId
import razie.Logging
import razie.diesel.Diesel
import razie.diesel.dom.RDOM.P
import razie.diesel.dom.{DieselAssets, RDomain}
import razie.diesel.engine.nodes._
import razie.diesel.expr._
import razie.diesel.model.DieselMsg
import razie.diesel.model.DieselMsg.ENGINE.{DIESEL_MSG_ACTION, DIESEL_MSG_ATTRS, DIESEL_MSG_EA, DIESEL_MSG_ENTITY}
import razie.diesel.utils.DomCollector
import razie.tconf.DSpec
import razie.wiki.admin.GlobalData
import razie.wiki.model.WID
import scala.Option.option2Iterable
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.Try

trait InfoNode // just a marker for useless info nodes

/** the engine: one flow = one engine = one actor
  *
  * this class is a generic engine, managing the execution of the nodes, starting from the root
  *
  * specific instances, like DomEngineV1 will take care of the actual execution of nodes. The two methods to
  * implement are:
  * - expand
  *
  *
  * The DomEngineExec is the actual implementation
  *
  * @param correlationId is parentEngineID.parentSuspendID - if dot is missing, this was a fire/forget
  *
  */
abstract class DomEngine(
  val dom: RDomain,
  val root: DomAst,
  val settings: DomEngineSettings,
  val pages: List[DSpec],
  val description: String,
  val correlationId: Option[String] = None,
  val id: String = new ObjectId().toString) extends Logging with DomEngineState with DomRoot with DomEngineExpander {

  assert(settings.realm.isDefined, "need realm defined for engine settings")

  GlobalData.dieselEnginesTotal.incrementAndGet()

  def collectGroup = settings.collectGroup.getOrElse(description)

  def wid = WID("DieselEngine", id)

  def href = DieselAssets.mkAhref(WID("DieselEngine", this.id))

  var synchronous = false
  implicit val engine: DomEngineState = this

  // setup the context for this eval
  implicit var ctx: ECtx = new DomEngECtx(settings)
      .withEngine(this)
      .withDomain(dom)
      .withSpecs(pages)
      .withCredentials(settings.userId)
      .withHostname(settings.node)

  /** myself */
  def href(format: String = "") = s"/diesel/engine/view/$id?format=$format"

  val rules = dom.moreElements.collect {
    case e: ERule => e
  }

  val flows = dom.moreElements.collect {
    case e: EFlow => e
  }

  // collecting warnings
  protected var warnings: Option[DomAst] = None

  /** collect warnings */
  def warning(warning: InfoNode) = {
    warnings.foreach(w =>
      evAppChildren(w, List(DomAst(warning, AstKinds.ERROR).withStatus(DomState.SKIPPED))))
  }

  //==========================

  /** make a static context for children */
  protected def mkMsgContext(in: Option[EMsg], attrs: List[P], parentCtx: ECtx, a: DomAst) = {
    new StaticECtx(
      in.map(in => P.fromTypedValue(DIESEL_MSG_ENTITY, in.entity)).toList :::
          in.map(in => P.fromTypedValue(DIESEL_MSG_ACTION, in.met)).toList :::
          in.map(in => P.fromTypedValue(DIESEL_MSG_EA, in.ea)).toList :::
          // todo avoid this every time - lazy parms ?
          List(P.fromTypedValue(DIESEL_MSG_ATTRS,
            attrs.map(p => (p.name, p.calculatedTypedValue(parentCtx).value)).toMap)) :::
          attrs,
      Some(parentCtx),
      Some(a))
  }

  /** spawn new engine */
  protected def spawn(nodes: List[DomAst], correlationId: Option[String]) = {
    val newRoot = DomAst("root", AstKinds.ROOT).withDetails("(spawned)")
    /* can't use evAppChildren*/
//    evAppChildren(newRoot, nodes)
    newRoot.appendAllNoEvents(nodes)
    val engine = DieselAppContext.mkEngine(dom, newRoot, settings, pages, "engine:spawn", correlationId)
    engine.ctx.root._hostname = ctx.root._hostname
    engine
  }

  /** if have correlationID, notify parent... */
  protected def notifyParent(a: DomAst, parentId: String, targetId: String, level: Int) = {
    // completed - notify parent
    val newD = DomAst(new EInfo(
      "Completing - notifying parent: " + DieselAssets.mkLink(WID("DieselEngine", parentId)), ""),
      AstKinds.GENERATED)

    DieselAppContext ! DEComplete(parentId, targetId, recurse = true, level,
      ctx.getp(Diesel.PAYLOAD).map(p => DomAst(EVal(p))).toList
    )

    evAppChildren(a, newD)
    true
  }

  /** process a list of continuations */
  protected def later(m: List[DEMsg]): List[Any] = {
    if (synchronous) {
      m.map(processDEMsg)
    } else
      m.map(m => DieselAppContext.router.map(_ ! m))
  }

  /** stop and discard this engine */
  def discard = {
    if (status != DomState.INIT) {
      // did something, need to stop it instead
      stopNow
    } else {
      clog << s"WF.DISCARD.$id"
      status = DomState.CANCEL
      trace("DomEng " + id + " discard")
      finishP.success(this)
      DieselAppContext.stopActor(id)
    }
  }

  /** finalize and release resources, actors etc */
  def engineDone() = {
    clog << s"WF.STOP.$id"
    GlobalData.dieselEnginesActive.decrementAndGet()
    finishP.success(this)
    DomCollector.collectAst("engine", settings.realm.mkString, id, settings.userId, this)

    // stop owned streams
    // todo get pissed if not done?
    this.ownedStreams.foreach(stream => {
      stream.cleanup()
    })
  }

  /** stop me now */
  def stopNow = {
    status = DomState.CANCEL
    trace("DomEng " + id + " stopNow")
    engineDone()
    DieselAppContext.stopActor(id)
  }

  /** completed a node - udpate stat
    *
    * @param a node that completed
    * @param level - current level, root is 0
    * @return
    */
  def nodeDone(a: DomAst, level:Int=1): List[DEMsg] = {
    evChangeStatus(a, DomState.DONE)
    a.end()

    trace("DomEng "+id+("  " * level)+" done " + a.value)

    // parent status update

    val x =
      checkState(findParent(a).getOrElse(root)) :::
          depys.toList.filter(_.prereq.id == a.id).flatMap(d =>
            root.find(d.depy.id).toList
          ).filter { n =>
            val prereq = depys.filter(x => x.depy.id == n.id && DomState.inProgress(x.prereq.status))
            prereq.isEmpty && n.status == DomState.DEPENDENT // not started - important!
          }.map { n =>
            evChangeStatus(n, DomState.LATER)
            DEReq(id, n, recurse = true,
              findLevel(n)) // depys are not even equals - they would have their own level...?
            // had a problem here, where the level was reseting sometimes - can't depend on parent level, have to
            // re-find it
          }
    x
  }

  /** find the next *async* execution list. this makes the sync/async determination, based on
    * dependencies, node type etc
    *
    * a is already assumed to have been stitched in the main tree
    *
    * @param parent  front from here down
    * @param results - the results returned from a parent's executor
    * @return
    */
  protected def findFront(parent: DomAst, results: List[DomAst]): List[DomAst] = {
    // any flows to shape it?
    var res = results

    // seq-par: returns the front of that branch and builds side effecting depys from those to the rest
    def rec(e: FlowExpr): List[DomAst] = e match {

      // sequential, create depys
      case SeqExpr(op, l) if op == "+" => {
        val res = rec(l.head)
        l.drop(1).foldLeft(res)((a, b) => {
          val bb = rec(b)
          crdep(a, bb)
          bb
        })
        res
      }

      // parallel, start them all
      case SeqExpr(op, l) if op == "|" => {
        val x = l.flatMap(rec).toList
        x
      }

      // more seq-par
      case BFlowExpr(ex) => rec(ex)

      case MsgExpr(ea) => parent.children.collect {
        case n: DomAst if
        n.value.isInstanceOf[EMsg] && n.value.asInstanceOf[EMsg].ea == ea ||
            n.value.isInstanceOf[ENext] && n.value.asInstanceOf[ENext].msg.ea == ea
        => n
      }

    }

    // any seq/par flows apply to a ? (a is the parent that generated these messages)
    findFlows(parent).map { f =>
      res = rec(f.ex)
    }

    // add explicit depys for results/children
    // note - the strategy to exec logically sync or async rests with the V1, not here - see who populates prereq
    // here we just respect them by creating depys
    results.filter(_.prereq.nonEmpty).map { res =>
      crdep(res.prereq.flatMap(x => root.find(x)), List(res))
    }

    res.filter(a => a.status != DomState.DEPENDENT && !DomState.isDone(a.status))
  }

  /** find any applicable flow directives */
  def findFlows(parent: DomAst) = {
    // any seq/par flows apply to a ? (a is the parent that generated these messages)
    if (parent.value.isInstanceOf[EMsg]) {
      implicit val ctx: StaticECtx = new StaticECtx(parent.value.asInstanceOf[EMsg].attrs, Some(this.ctx), Some(parent))
      flows.filter(_.e.test(parent, parent.value.asInstanceOf[EMsg]))
    } else
      Nil
  }


  /** a decomp req - starts processing a message. these can be deferred async or recurse synchronously
    *
    * @param a       the node to decompose/process
    * @param recurse recurse or not
    * @param level   - current level, root is 0
    */
  protected def req(a: DomAst, recurse: Boolean = true, level: Int) {
    var msgs: List[DEMsg] = Nil

    if(!DomState.isDone(a.status)) { // may have been skipped by others
      evChangeStatus(a, DomState.STARTED)
      a.start(seq())

      trace("DomEng " + id + ("  " * level) + " expand " + a.value)
      msgs = Try {
        expand(a, recurse, level + 1)
      }.recover {
        case t if t.isInstanceOf[javax.script.ScriptException] || t.isInstanceOf[DieselExprException] => {
          info("Exception wile decompose() " + t.getMessage)
          val err = DEError(this.id, t.toString)
          val ast = DomAst(
            new EError("Exception: " + t.getMessage, t)
          ).withStatus(DomState.DONE)

          evAppChildren(a, ast)
          err :: nodeDone(ast, level + 1)
        }
        case t: Throwable => {
          razie.Log.log("Exception wile decompose()", t)
          val err = DEError(this.id, t.toString)
          val ast = DomAst(new EError("Exception " + t.getMessage, t)).withStatus(DomState.DONE)

          evAppChildren(a, ast)
          err :: nodeDone(ast, level + 1)
        }
      }.get

      if (a.value.isInstanceOf[EEngSuspendDaemon]) {
        // nop
        evChangeStatus(a, DomState.SUSPENDED)
      } else if (a.value.isInstanceOf[EEngSuspend]) {
        // nop
        evChangeStatus(a, DomState.SUSPENDED)
      } else if (msgs.isEmpty)
        msgs = msgs ::: nodeDone(a, level)
    }

    checkState()
    later(msgs)
  }

  /** process a reply with results
    *
    * will decompose this node into children and spawn async those that need it
    *
    * @param a       the node that got this reply
    * @param recurse do i need to recurse into children? default=true
    * @param level   - current level, 0 is first
    * @param results - the results
    * @return
    */
  def rep(a: DomAst, recurse: Boolean = true, level: Int, results: List[DomAst], justAddChildren: Boolean = false)
  : List[DEMsg] = {
    var msgs: List[DEMsg] = Nil // continuations from this cycle

    evAppChildren(a, results)

    // can't do this - too stupid. it's easy to have stories with 100 or more activities
//    if (a.children.size >= maxLevels) {
    // simple protection against infinite loops
//      evAppChildren(a, DomAst(TestResult("fail: Max-Children!", "You have a loop rule out of control ( > 20 loops).
//      .."), "error"))
//    } else
    if (recurse) {
      msgs =
          msgs :::
              findFront(a, results).map { n =>
                // start the next child
                // todo this would be sync:     req(n, recurse, level + 1)i
                evChangeStatus(n, DomState.LATER)
                DEReq(id, n, recurse, level + 1) // increase level for children
              }
    }

    msgs
  }

  /** check the state at end of step - anything still running?
    *
    * @return list of continuations triggered by this state change
    */
  protected def checkState(node: DomAst = root) = {
    val res = node.collect {
      case a if a.id != node.id && DomState.inProgress(a.status) => a
    }

    var result: List[DEMsg] = Nil

    if (res.size > 0) {
      if (node.status != DomState.SUSPENDED) {
        // suspended nodes stay in SUSPENDED. their children can continue tho
        evChangeStatus(node, DomState.STARTED)
        node.end()
      }
    } else if (
      node.status != DomState.SUSPENDED ||
          node.childrenCol.exists(child =>
            child.value.isInstanceOf[EEngComplete] &&
                DomState.isDone(child.status)
          )
    ) {
      // only if they have a completed EEngComplete can complete a SUSPENDED

      if (node.status != DomState.DONE) {
        // call only when transitioning to DONE
        result = nodeDone(node) //node.status = DONE
      }
    }

    // when all done, the entire engine is done

    if (root.status == DomState.DONE && status != DomState.DONE) {
      status = DomState.DONE
      trace("DomEng " + id + " finish")
      engineDone()
      DieselAppContext.activeActors.get(id).foreach(_ ! DEStop) // stop the actor and remove engine

    }

    result
  }

  /** add built-in triggers and rules
    *
    * IT IS important that this be called just before starting execution, after all children were
    * added to the root
    *
    * @param l children alredy added tothe root
    */
  protected def prepRoot(l: ListBuffer[DomAst]): ListBuffer[DomAst] = {
    val vals = DomAst(EMsg(DieselMsg.ENGINE.DIESEL_VALS), AstKinds.TRACE)
    val warns = DomAst(EMsg(DieselMsg.ENGINE.DIESEL_WARNINGS), AstKinds.TRACE)
    val before = DomAst(EMsg(DieselMsg.ENGINE.DIESEL_BEFORE), AstKinds.TRACE)
    val after = DomAst(EMsg(DieselMsg.ENGINE.DIESEL_AFTER), AstKinds.TRACE)
    val desc = DomAst(EInfo(
      description,
      this.pages.map(_.specRef.wpath).mkString("\n") + "\n" + this.settings.toString
    ), AstKinds.DEBUG).withStatus(DomState.SKIPPED)

    // create dependencies and add them to the list
    after.prereq = l.map(_.id).toList
    root.appendAllNoEvents(List(after))
    l.foreach(x => x.prereq = before.id :: x.prereq)

    root.prependAllNoEvents(List(desc, vals, before, warns))

    warnings = Some(warns)

    //child engines notify parent if needed
    correlationId
        .filter(_ contains ".")
        .foreach(cid => {
          val ids = cid.split("\\.")
          val notifyParent = DomAst(EMsg(
            DieselMsg.ENGINE.DIESEL_PONG,
            List(
              P.of("parentId", ids(0)),
              P.of("targetId", ids(1)),
              P.of("level", -1)
            )
          ))

          // dead last node
          notifyParent.prereq = l.map(_.id).toList
          root.appendAllNoEvents(List(notifyParent))
        })
    l
  }

  /**
    * process only the tests - in sequence, this is meant to validate a data from another engine
    *
    * we will remove all other nodes but the $expect from the story
    *
    * this will prep the root node, then start execution
    */
  def processTests = {
    Future {
      GlobalData.dieselEnginesActive.incrementAndGet()
      clog << s"WF.START.$id"
      trace("*******************************************************")
      trace("DomEng.tests STARTING " + id + " - " + description)
      trace("*******************************************************")
      trace("")

      root.status = DomState.STARTED
      this.status = DomState.STARTED
      root.start(seq())

      prepRoot(
        root
            .childrenCol
            .filter(_.kind == "test")
      ).foreach(expand(_, recurse = true, 1))

      this
    }
  }

  // waitingo for flow to complete
  val finishP = Promise[DomEngine]()
  val finishF = finishP.future

  /**
    * process the root
    *
    * this will prep the root node, then start execution
    */
  def process: Future[DomEngine] = {
    if (root.status != DomState.STARTED) {

      GlobalData.dieselEnginesActive.incrementAndGet()
      clog << s"WF.START.$id"
      trace("*******************************************************")
      trace("DomEng STARTING " + id + " - " + description)
      trace("*******************************************************")
      trace("")
      root.status = DomState.STARTED
      this.status = DomState.STARTED
      root.start(seq())

      // async start
      //    val msgs = root.children.toList.map{x=>
      val msgs = findFront(root, prepRoot(root.childrenCol).toList).map { x =>
        x.status = DomState.LATER
        DEReq(id, x, recurse = true, 0)
      }

      if (msgs.isEmpty)
        nodeDone(root)
      later(msgs)
    }

    finishF
  }

  // todo is it really needed?
  def execSync(ast:DomAst, level:Int, ctx: ECtx) : Option[P] = {
    // stop propagation of local vals to parent engine
    var newCtx:ECtx = new ScopeECtx(Nil, Some(ctx), Some(ast))
    // include this messages' context
    newCtx = new StaticECtx(ast.value.asInstanceOf[EMsg].attrs, Some(newCtx), Some(ast))

    // inherit all context from parent engine
//    this.ctx.root.overwrite(newCtx)

    val msgs = expandEMsg(ast, ast.value.asInstanceOf[EMsg], recurse = true, level, newCtx)

    evAppChildren(ast, msgs)

    var res = newCtx.getp("payload")
    msgs.collect {
      case a if a.value.isInstanceOf[EMsg] =>
        res = execSync(a, level + 1, newCtx)
//      case a if a.value.isInstanceOf[EVal] =>
//        res = execSync(a, level + 1, newCtx)
    }
    ast.status = DomState.DONE
    ast.end()
    res
  }

  /** main processing of next - called from actor in async and in thread when sync/decompose */
  private[engine] def processDEMsg(m: DEMsg) = {
    m match {
      case DEReq(eid, a, r, l) => {
        require(eid == this.id) // todo logical error not a fault
        this.req(a, r, l)
      }

      case DERep(eid, a, r, l, results) => {
        require(eid == this.id) // todo logical error not a fault
        this.rep(a, r, l, results)
      }

      case DEComplete(eid, aid, r, l, results) => {
        require(eid == this.id) // todo logical error not a fault
        val target = n(aid)
        val completer = DomAst(EEngComplete("DEComplete"), AstKinds.BUILTIN)
        val toAdd = results ::: completer :: Nil
        // don't trust them, find level
        val level = Try {findLevel(target)}.getOrElse(l)
        if (target.status != DomState.DONE) {
          evAppChildren(target, toAdd)
          //          val msgs = nodeDone(n(aid), level + 1)
          val msgs = nodeDone(completer, level + 1)
          checkState()
          later(msgs)
        } else {
          // todo add a warn
          evAppChildren(
            target,
            List(DomAst(EWarning("DEComplete on DONE"), AstKinds.DEBUG))
          )

          Nil
        }
      }

      case DEAddChildren(eid, aid, r, l, results) => {
        // add more children to a node (used for async prcs, like stream consumption)
        require(eid == this.id) // todo logical error not a fault
        val target = n(aid)

        // don't trust them, find level
        val level = Try {findLevel(target)}.getOrElse(l)

        later(
          this.rep(target, true, level + 1, results)
        )
      }
    }
  }

  /** extract the resulting values from this engine */
  def extractValues (e:String, a:String) = {
    require(DomState.isDone(this.status)) // no sync

    // find the spec and check its result
    // then find the resulting value.. if not, then json
    val oattrs = dom.moreElements.collectFirst {
      case n: EMsg if n.entity == e && n.met == a => n
    }.toList.flatMap(_.ret)

    // collect values
    val valuesp = root.collect {
      case d@DomAst(EVal(p), /*AstKinds.GENERATED*/ _, _, _) if oattrs.isEmpty || oattrs.find(_.name == p.name).isDefined => p
    }

    valuesp
  }

  /** this was resulted in an html response - add it as a trace */
  def addResponseInfo (code: Int, body:String, headers:Map[String, String]): Unit = {
    val h = headers.mkString("\n")
    val ast = new DomAst(EInfo(s"Response: ${code} BODY: ${body.take(500)}", s"HEADERS:\n$h"), AstKinds.TRACE)
    this.root.appendAllNoEvents(List(ast))
  }

  /** extract the resulting value from this engine
    * extract one value - try:
    * 1. defined response oattrs
    * 2. last valuep - the last message produced in the flow
    * 3. payload
    *
    * @param ea = initial message ea if known, empty otherwise
    * */
  def extractFinalValue(ea: String, evenIfExecuting: Boolean = false): Option[P] = {
    if (!evenIfExecuting) require(DomState.isDone(this.status)) // no sync

    // find the spec and check its result
    // then find the resulting value.. if not, then json
    val oattrs = dom.moreElements.collectFirst {
      case n: EMsg if n.ea == ea => n
    }.toList.flatMap(_.ret)

    //    if (oattrs.isEmpty) {
    //      errors append s"Can't find the spec for $msg"
    //    }

    // collect all values
    // todo this must be smarter - ignore diese.before values, don't look inside scopes etc?
    val valuesp = root.collect {
      case d@DomAst(EVal(p), /*AstKinds.GENERATED*/ _, _, _) if oattrs.isEmpty || oattrs.find(_.name == p.name).isDefined => p
    }

    // extract one value - try:
    // 1. defined response oattrs
    // 2. last valuep - the last message produced in the flow
    // 3. payload
    val resp = oattrs.headOption.flatMap(oa=> valuesp.find(_.name == oa.name))
      .orElse(valuesp.lastOption)
      .orElse(valuesp.find(_.name == Diesel.PAYLOAD))

    resp
  }

  def finalContext (e:String, a:String) = {
    require(DomState.isDone(this.status)) // no sync

    // find the spec and check its result
    // then find the resulting value.. if not, then json
    val oattrs = dom.moreElements.collectFirst {
      case n: EMsg if n.entity == e && n.met == a => n
    }.toList.flatMap(_.ret)

    // collect values
    val values = root.collect {
      case DomAst(EVal(p), /*AstKinds.GENERATED*/ _, _, _) if oattrs.isEmpty || oattrs.find(_.name == p.name).isDefined => p
    }

    values
  }

}

