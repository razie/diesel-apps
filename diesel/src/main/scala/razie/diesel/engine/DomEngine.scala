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
import razie.diesel.dom.RDomain
import razie.diesel.engine.nodes._
import razie.diesel.expr._
import razie.diesel.model.DieselMsg
import razie.diesel.utils.DomCollector
import razie.tconf.DSpec
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
  */
abstract class DomEngine(
  val dom: RDomain,
  val root: DomAst,
  val settings: DomEngineSettings,
  val pages : List[DSpec],
  val description:String,
  val id:String = new ObjectId().toString) extends Logging with DomEngineState with DomRoot with DomEngineExpander {

  assert(settings.realm.isDefined, "need realm defined for engine settings")

  var synchronous = false
  implicit val engine = this

  // setup the context for this eval
  implicit var ctx : ECtx = new DomEngECtx(settings)
    .withEngine(this)
    .withDomain(dom)
    .withSpecs(pages)
    .withCredentials(settings.userId)
    .withHostname(settings.node)

  /** myself */
  def href (format:String="") = s"/diesel/engine/view/$id?format=$format"

  val rules = dom.moreElements.collect {
    case e:ERule => e
  }

  val flows = dom.moreElements.collect {
    case e:EFlow => e
  }

  //==========================

  /** spawn new engine */
  protected def spawn (nodes:List[DomAst]) = {
    val newRoot = DomAst("root", AstKinds.ROOT).withDetails("(spawned)")
    evAppChildren(newRoot, nodes)
    val engine = DieselAppContext.mkEngine(dom, newRoot, settings, pages, "engine:spawn")
    engine.ctx.root._hostname = ctx.root._hostname
    engine
  }

  /** process a list of continuations */
  protected def later (m:List[DEMsg]): List[Any] = {
    if(synchronous) {
      m.map(processDEMsg)
    } else
      m.map(m=>DieselAppContext.router.map(_ ! m))
  }

  /** stop and discard this engine */
  def discard = {
    status = DomState.CANCEL
    trace("DomEng "+id+" discard")
    finishP.success(this)
    DieselAppContext.stopActor(id)
  }

  /** stop me now */
  def stopNow = {
      status = DomState.CANCEL
      trace("DomEng "+id+" stopNow")
      DomCollector.collectAst ("engine", settings.realm.mkString, id, settings.userId, this)
      finishP.success(this)
      DieselAppContext.stopActor(id)
  }

  /** completed a node - udpate stat
    *
    * @param a node that completed
    * @param level
    * @return
    */
  def nodeDone(a: DomAst, level:Int=1): List[DEMsg] = {
    evChangeStatus(a, DomState.DONE)
    a.end

    trace("DomEng "+id+("  " * level)+" done " + a.value)

    // parent status update

    val x =
      checkState(findParent(a).getOrElse(root)) :::
      depys.toList.filter(_.prereq.id == a.id).flatMap(d=>
        root.find(d.depy.id).toList
      ).filter { n =>
        val prereq = depys.filter(x => x.depy.id == n.id && DomState.inProgress(x.prereq.status))
        prereq.isEmpty && n.status == DomState.DEPENDENT // not started - important!
      }.map { n =>
        evChangeStatus(n, DomState.LATER)
        DEReq(id, n, true, findLevel(n)) // depys are not even equals - they would have their own level...?
        // had a problem here, where the level was reseting sometimes - can't depend on parent level, have to re-find it
      }
    x
  }

  /** find the next *async* execution list. this makes the sync/async determination, based on
    * dependencies, node type etc
    *
    * a is already assumed to have been stitched in the main tree
    *
    * @param a
    * @param results - the results returned from a's executor
    * @return
    */
  protected def findFront(a: DomAst, results:List[DomAst]): List[DomAst] = {
    // any flows to shape it?
    var res = results

    // seq-par: returns the front of that branch and builds side effecting depys from those to the rest
    def rec(e:FlowExpr) : List[DomAst] = e match {

        // sequential, create depys
      case SeqExpr(op, l) if op == "+" => {
        val res = rec(l.head)
        l.drop(1).foldLeft(res)((a,b)=> {
          val bb = rec(b)
          crdep(a,bb)
          bb
        })
        res
      }

        // parallel, start them all
      case SeqExpr(op, l) if op == "|" => {
        l.flatMap(rec).toList
      }

      case MsgExpr(ea) => a.children.collect {
        case n:DomAst if n.value.isInstanceOf[EMsg] && n.value.asInstanceOf[EMsg].entity+"."+n.value.asInstanceOf[EMsg].met == ea => n
      }.toList

        // more seq-par
      case BFlowExpr(ex) => rec(ex)
    }

    // any seq/par flows apply to a ?
    if(a.value.isInstanceOf[EMsg]) {
      implicit val ctx = new StaticECtx(a.value.asInstanceOf[EMsg].attrs, Some(this.ctx), Some(a))
      flows.filter(_.e.test(a.value.asInstanceOf[EMsg])).map { f =>
        res = rec(f.ex)
      }
    }

    // add explicit depys for results/children
    results.filter(_.prereq.nonEmpty).map{res=>
      crdep(res.prereq.flatMap(x=> root.find(x)), List(res))
    }

    res
        .filter(a=> a.status != DomState.DEPENDENT && !DomState.isDone(a.status))
  }

  /** a decomp req - starts processing a message. these can be deferred async or recurse synchronously
    *
    * @param a the node to decompose/process
    * @param recurse
    * @param level
    */
  protected def req(a: DomAst, recurse: Boolean = true, level: Int): Unit = {
    var msgs : List[DEMsg] = Nil

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
          val ast = DomAst(new EError("Exception", t)).withStatus(DomState.DONE)

          evAppChildren(a, ast)
          err :: nodeDone(ast, level + 1)
        }
      }.get

      if (a.value.isInstanceOf[EEngSuspend]) {
        // nop
        evChangeStatus(a, DomState.LATER)
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
    * @param a the node that got this reply
    * @param recurse do i need to recurse into children? default=true
    * @param level
    * @param results - the results
    * @return
    */
  def rep(a: DomAst, recurse: Boolean = true, level: Int, results:List[DomAst]): List[DEMsg] = {
    var msgs: List[DEMsg] = Nil // continuations from this cycle

    evAppChildren(a, results)

    // can't do this - too stupid. it's easy to have stories with 100 or more activities
//    if (a.children.size >= maxLevels) {
      // simple protection against infinite loops
//      evAppChildren(a, DomAst(TestResult("fail: Max-Children!", "You have a loop rule out of control ( > 20 loops)..."), "error"))
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

  // check the state at end of step - anything still running?
  protected def checkState (node:DomAst = root) = {
    val res = node.collect {
      case a if a.id != node.id && DomState.inProgress(a.status) => a
    }

    var result : List[DEMsg] = Nil

    if(res.size > 0) {
      evChangeStatus(node, DomState.STARTED)
      node.end
    } else {
      if(node.status != DomState.DONE)
        // call only when transitioning to DONE
        result = nodeDone(node) //node.status = DONE
    }

    // when all done, the entire engine is done

    if(root.status == DomState.DONE && status != DomState.DONE) {
      status = DomState.DONE
      trace("DomEng "+id+" finish")
      DomCollector.collectAst ("engine", settings.realm.mkString, id, settings.userId, this)
      finishP.success(this)
      DieselAppContext.activeActors.get(id).foreach(_ ! DEStop) // stop the actor and remove engine
    }

    result
  }

  /** add built-in triggers */
  protected def prepRoot(l:ListBuffer[DomAst]) : ListBuffer[DomAst] = {
    val vals = DomAst(EMsg(DieselMsg.ENGINE.DIESEL_VALS), AstKinds.TRACE)
    val before = DomAst(EMsg(DieselMsg.ENGINE.DIESEL_BEFORE), AstKinds.TRACE)
    val after = DomAst(EMsg(DieselMsg.ENGINE.DIESEL_AFTER), AstKinds.TRACE)
    val desc = (DomAst(EInfo(
      description,
      this.pages.map(_.specPath.wpath).mkString("\n") + "\n" + this.settings.toString
    ), AstKinds.DEBUG).withStatus(DomState.SKIPPED))

    // create dependencies and add them to the list
    after.prereq = l.map(_.id).toList
    l.append(after)
    l.map(x=> x.prereq = before.id :: x.prereq)
    l.prepend(before)
    l.prepend(vals)
    l.prepend(desc)

    l
  }

  /** process only the tests - synchronously */
  def processTests = {
    Future {
      root.status = DomState.STARTED
      this.status = DomState.STARTED
      root.start(seq())

      prepRoot(
        root
          .childrenCol
          .filter(_.kind == "test")
      ).foreach(expand(_, true, 1))

      this
    }
  }

  // waitingo for flow to complete
  val finishP = Promise[DomEngine]()
  val finishF = finishP.future

  def process : Future[DomEngine] = {
    if(root.status != DomState.STARTED) {

      trace( "DomEng " + id + " process root: " + root.value)
      root.status = DomState.STARTED
      this.status = DomState.STARTED
      root.start(seq())

      // async start
      //    val msgs = root.children.toList.map{x=>
      val msgs = findFront(root, prepRoot(root.childrenCol).toList).map { x =>
        x.status = DomState.LATER
        DEReq(id, x, true, 0)
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

    val msgs = expandEMsg(ast, ast.value.asInstanceOf[EMsg], true, level, newCtx)

    evAppChildren(ast, msgs)

    var res = newCtx.getp("payload")
    msgs.collect {
      case a if a.value.isInstanceOf[EMsg] =>
        res = execSync(a, level + 1, newCtx)
//      case a if a.value.isInstanceOf[EVal] =>
//        res = execSync(a, level + 1, newCtx)
    }
    ast.status = DomState.DONE
    ast.end
    res
  }

  /** main processing of next - called from actor in async and in thread when sync/decompose */
  private[engine] def processDEMsg (m:DEMsg) = {
    m match {
      case DEReq(eid, a, r, l) => {
        require(eid == this.id) // todo logical error not a fault
        this.req(a, r, l)
      }

      case DERep(eid, a, r, l, results) => {
        require(eid == this.id) // todo logical error not a fault
        this.rep(a, r, l, results)
      }

      case DEComplete(eid, a, r, l, results) => {
        require(eid == this.id) // todo logical error not a fault
        val msgs = nodeDone(a, l)
        checkState()
        later(msgs)
      }
    }
  }

  /** extract the resulting values from this engine */
  def extractValues (e:String, a:String) = {
    require(DomState.isDone(this.status)) // no sync

    // find the spec and check its result
    // then find the resulting value.. if not, then json
    val oattrs = dom.moreElements.collect {
      case n: EMsg if n.entity == e && n.met == a => n
    }.headOption.toList.flatMap(_.ret)

    // collect values
    val valuesp = root.collect {
      case d@DomAst(EVal(p), /*AstKinds.GENERATED*/ _, _, _) if oattrs.isEmpty || oattrs.find(_.name == p.name).isDefined => p
    }

    valuesp
  }

  /** this was resulted in an html response - add it as a trace */
  def addResponseInfo (code: Int, body:String, headers:Map[String, String]) = {
    val h = headers.mkString("\n")
    val ast = new DomAst(EInfo(s"Response: ${code} BODY: ${body.take(500)}", s"HEADERS:\n$h"), AstKinds.TRACE)
    this.root.childrenCol.append(ast)
  }

  /** extract the resulting value from this engine
    * extract one value - try:
    * 1. defined response oattrs
    * 2. last valuep - the last message produced in the flow
    * 3. payload
    * */
  def extractFinalValue (e:String, a:String) = {
    require(DomState.isDone(this.status)) // no sync

    // find the spec and check its result
    // then find the resulting value.. if not, then json
    val oattrs = dom.moreElements.collect {
      case n: EMsg if n.entity == e && n.met == a => n
    }.headOption.toList.flatMap(_.ret)

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
    val oattrs = dom.moreElements.collect {
      case n: EMsg if n.entity == e && n.met == a => n
    }.headOption.toList.flatMap(_.ret)

    // collect values
    val values = root.collect {
      case d@DomAst(EVal(p), /*AstKinds.GENERATED*/ _, _, _) if oattrs.isEmpty || oattrs.find(_.name == p.name).isDefined => p
    }

    values
  }

}


