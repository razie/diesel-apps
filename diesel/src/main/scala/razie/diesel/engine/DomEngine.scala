/**
 *  ____    __    ____  ____  ____  ___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

import org.bson.types.ObjectId
import org.joda.time.DateTime
import razie.diesel.dom.RDOM.{P, PValue}
import razie.diesel.dom.{DomState, RDomain, _}
import razie.diesel.engine.RDExt._
import razie.diesel.exec.{EApplicable, Executors}
import razie.diesel.expr.DieselExprException
import razie.diesel.ext.{BFlowExpr, FlowExpr, MsgExpr, SeqExpr, _}
import razie.diesel.model.DieselMsg
import razie.diesel.utils.DomCollector
import razie.tconf.DSpec
import razie.wiki.parser.PAS
import razie.{Logging, js}
import scala.Option.option2Iterable
import scala.collection.mutable.{ArrayBuffer, HashMap, ListBuffer}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.Try

/** DDD - base trait for events */
trait DEvent {
  def dtm : DateTime
}

/** DDD - node expanded */
case class DEventExpNode(nodeId:String, children:List[DomAst], dtm:DateTime=DateTime.now) extends DEvent with CanHtml {
  override def toHtml = s"EvExpNode: $nodeId, ${children.map(_.toString).mkString}"
}

/** DDD - node status */
case class DEventNodeStatus(nodeId:String, status:String, dtm:DateTime=DateTime.now) extends DEvent with CanHtml {
  override def toHtml = s"EvNodeStatus: $nodeId, $status}"
}

/** dependency between two nodes */
case class DADepy (prereq:DomAst, depy:DomAst)

/** dependency event */
case class DADepyEv (prereq:String, depy:String, dtm:DateTime=DateTime.now) extends DEvent


/** the engine: one flow = one engine = one actor */
class DomEngine(
  val dom: RDomain,
  val root: DomAst,
  val settings: DomEngineSettings,
  val pages : List[DSpec],
  val description:String,
  val id:String = new ObjectId().toString) extends Logging {

  assert(settings.realm.isDefined, "need realm defined for engine settings")

  var status = DomState.INIT
  var synchronous = false
  var seqNo = 0 // each node being processed in sequence, gets a stamp here, so you can debug what runs in parallel
  def seq() = {
    val t = seqNo
    seqNo = seqNo +1
    t
  }

  implicit val engine = this

  final val maxLevels = 45
  final val maxExpands= 10000
  var curExpands = 0

  // setup the context for this eval
  implicit var ctx : ECtx = new DomEngECtx(settings)
    .withEngine(this)
    .withDomain(dom)
    .withSpecs(pages)
    .withCredentials(settings.userId)
    .withHostname(settings.node)

  /** myself */
  def href (format:String="") = s"/diesel/engine/view/$id?format=$format"

  def failedTestCount = root.failedTestCount
  def errorCount = root.errorCount
  def successTestCount = root.successTestCount
  def totalTestCount = root.totalTestCount
  def progress:String = root.totalTestCount + "/" + root.todoTestCount
  def totalCount = root.totalTestCount

  /** collect generated values */
  def resultingValues() = root.collect {
    // todo see in Api.irunDom, trying to match them to the message sent in...
    case d@DomAst(EVal(p), /*AstKinds.GENERATED*/ _, _, _) /*if oattrs.isEmpty || oattrs.find(_.name == p.name).isDefined */ => (p.name, p.currentStringValue)
  }

  /** collect the last generated value OR empty string */
  def resultingValue = root.collect {
    case d@DomAst(EVal(p), /*AstKinds.GENERATED*/ _, _, _) /*if oattrs.isEmpty || oattrs.find(_.name == p.name).isDefined */ => (p.name, p.currentStringValue)
  }.lastOption.map(_._2).getOrElse("")

  val rules = dom.moreElements.collect {
    case e:ERule => e
  }

  val flows = dom.moreElements.collect {
    case e:EFlow => e
  }

  //========================== DDD

  val events : ListBuffer[DEvent] = new ListBuffer[DEvent]()

  def addEvent(e:DEvent*) : Unit = {
    events.append(e:_*)

    def n(id:String) = root.find(id).get

    e collect {

      case DEventExpNode(parentId, children, _) => {
        n(parentId).children appendAll children

        this.curExpands = curExpands + 1

      }

      case DEventNodeStatus(parentId, status, _) =>
        n(parentId).status = status

      case DADepyEv(pId, dId, _) =>
        depys.append(DADepy(n(pId),n(dId)))
    }
  }

  // todo it's faster here where i have the node handles than looking it up by id above...

  def evAppChildren (parent:DomAst, children:DomAst) : Unit =
    evAppChildren(parent, List(children))

  def evAppChildren (parent:DomAst, children:List[DomAst]) : Unit = {
    addEvent(DEventExpNode(parent.id, children))
  }

  def evChangeStatus (node:DomAst, status:String) : Unit = {
//    node.status = status
    addEvent(DEventNodeStatus(node.id, status))
  }

  def evAddDepy (p:DomAst, d:DomAst) : Unit = {
//    depys.append(DADepy(p,d))
    addEvent(DADepyEv(p.id,d.id))
  }

  //==========================

  /** spawn new engine */
  def spawn (nodes:List[DomAst]) = {
    val newRoot = DomAst("root", AstKinds.ROOT).withDetails("(spawned)")
    evAppChildren(newRoot, nodes)
    val engine = DieselAppContext.mkEngine(dom, newRoot, settings, pages, "engine:spawn")
    engine.ctx.root._hostname = ctx.root._hostname
    engine
  }

  /** process a list of continuations */
  def later (m:List[DEMsg]): List[Any] = {
    if(synchronous) {
      m.map(processDEMsg)
    } else
      m.map(m=>DieselAppContext.router.map(_ ! m))
  }

  /** stop me now */
  def stopNow = {
      status = DomState.DONE
      trace("DomEng "+id+" stopNow")
      DomCollector.collectAst ("engine", settings.realm.mkString, id, settings.userId, this)
      finishP.success(this)
      DieselAppContext.stopActor(id)
  }

  def collectValues[T] (f: PartialFunction[Any, T]) : List[T] =
    root.collect {
      case v if(f.isDefinedAt(v.value)) => f(v.value)
    }

  private def findParent(node: DomAst): Option[DomAst] =
    root.collect {
      case a if a.children.exists(_.id == node.id) => a
    }.headOption

  def findLevel(node: DomAst): Int =
    root.collect2 {
      case t@(a,l) if a.id == node.id => l
    }.headOption.getOrElse {
      throw new IllegalArgumentException("Can't find level for "+node)
    }

  /** completed a node - udpate stat
    *
    * @param a node that completed
    * @param level
    * @return
    */
  def done(a: DomAst, level:Int=1): List[DEMsg] = {
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

  /** dependencies */
  val depys = ListBuffer[DADepy]()

  /** create / add some dependencies, so d waits for p */
  def crdep(p:List[DomAst], d:List[DomAst]) = {
    d.map{d=>
      // d will wait
      if(p.nonEmpty) evChangeStatus(d, DomState.DEPENDENT)

      p.map{p=>
        evAddDepy(p,d)
      }
    }
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
  def findFront(a: DomAst, results:List[DomAst]): List[DomAst] = {
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
      crdep(res.prereq.flatMap(root.find), List(res))
    }

    res.filter(_.status != DomState.DEPENDENT)
  }

  /** a decomp req - starts processing a message. these can be deferred async or recurse synchronously
    *
    * @param a the node to decompose/process
    * @param recurse
    * @param level
    */
  def req(a: DomAst, recurse: Boolean = true, level: Int): Unit = {
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
            new EError("Exception: " + t.getMessage)
          ).withStatus(DomState.DONE)

          evAppChildren(a, ast)
          err :: done(ast, level + 1)
        }
        case t: Throwable => {
          razie.Log.log("Exception wile decompose()", t)
          val err = DEError(this.id, t.toString)
          val ast = DomAst(new EError("Exception", t)).withStatus(DomState.DONE)

          evAppChildren(a, ast)
          err :: done(ast, level + 1)
        }
      }.get

      if (a.value.isInstanceOf[EEngSuspend]) {
        // nop
        evChangeStatus(a, DomState.LATER)
      } else if (msgs.isEmpty)
        msgs = msgs ::: done(a, level)
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
  private def checkState (node:DomAst = root) = {
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
        result = done(node) //node.status = DONE
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
  private def prepRoot(l:ListBuffer[DomAst]) : ListBuffer[DomAst] = {
    val vals = DomAst(EMsg(DieselMsg.ENGINE.ENTITY, DieselMsg.ENGINE.VALS), AstKinds.TRACE)
    val before = DomAst(EMsg(DieselMsg.ENGINE.ENTITY, DieselMsg.ENGINE.BEFORE), AstKinds.TRACE)
    val after = DomAst(EMsg(DieselMsg.ENGINE.ENTITY, DieselMsg.ENGINE.AFTER), AstKinds.TRACE)

    // create dependencies and add them to the list
    after.prereq = l.map(_.id).toList
    l.append(after)
    l.map(x=> x.prereq = before.id :: x.prereq)
    l.prepend(before)
    l.prepend(vals)

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
          .children
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
      val msgs = findFront(root, prepRoot(root.children).toList).toList.map { x =>
        x.status = DomState.LATER
        DEReq(id, x, true, 0)
      }

      if (msgs.isEmpty)
        done(root)
      later(msgs)
    }

    finishF
  }

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
  def expand(a: DomAst, recurse: Boolean = true, level: Int): List[DEMsg] = {
    var msgs : List[DEMsg] = Nil // continuations from this cycle

    if (level >= maxLevels) {
      evAppChildren(a, DomAst(TestResult("fail: Max-Recurse!", "You have a recursive rule generating this branch..."), "error"))
      return Nil
    }

    if(this.curExpands > maxExpands) {
      // THIS IS OK: append direct to children
      a.children append DomAst(TestResult("fail: Max-Expand!", s"You have expanded too many nodes (>$maxExpands)..."), "error")
      return Nil //stopNow
    }
    // link the spec - some messages get here without a spec, because the DOM is not available when created
    if(a.value.isInstanceOf[EMsg] && a.value.asInstanceOf[EMsg].spec.isEmpty) {
      val m = a.value.asInstanceOf[EMsg]
      val spec = dom.moreElements.collect {
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

      case next@ENext(m, "==>", cond, _) => {
        // forking new engine / async branch
        implicit val ctx = new StaticECtx(next.parent.map(_.attrs).getOrElse(Nil), Some(this.ctx), Some(a))
        // start new engine/process
        // todo should find settings for target service  ?
        val eng = spawn(List(DomAst(next.evaluateMsg)))
        eng.process // start it up in the background
        evAppChildren(a, DomAst(EInfo(s"""Spawn engine <a href="/diesel/engine/view/${eng.id}">${eng.id}</a>""")))//.withPos((m.get.pos)))
        // no need to return anything - children have been decomposed
      }

      case n1@ENext(m, ar, cond, _) if "-" == ar || "=>" == ar => {
        // message executed later
        // todo bubu: static parent parms overwrite side effects updated in this.ctx
        implicit val ctx = new StaticECtx(n1.parent.map(_.attrs).getOrElse(Nil), Some(this.ctx), Some(a))

        if(n1.test()) {
          val newmsg = n1.evaluateMsg
          val newnode = DomAst(newmsg, AstKinds.kindOf(newmsg.arch))
          msgs = rep(a, recurse, level, List(newnode))
        }
        // message will be evaluate() later
      }

      case n1@ENextPas(m, ar, cond, _) if "-" == ar || "=>" == ar => {
        implicit val ctx = new StaticECtx(n1.parent.map(_.attrs).getOrElse(Nil), Some(this.ctx), Some(a))

        if(n1.test()) {
          appendValsPas (a, m, m.attrs, ctx)
        }
      }

      case x: EMsg if x.entity == "" && x.met == "" => {
        // just expressions, generate values from each attribute
        appendVals (a, x, x.attrs, ctx, AstKinds.kindOf(x.arch))
      }

      case in: EMsg => {
        // todo should collect all parent contexts from root to here...
//        implicit val parentCtx = new StaticECtx(in.attrs, Some(this.ctx), Some(a))
        msgs = expandEMsg(a, in, recurse, level, this.ctx) ::: msgs
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

  // todo is it really needed?
  def execSync(ast:DomAst, level:Int, ctx: ECtx) : Option[P] = {
    // stop propagation of local vals to parent engine
    val newCtx = new ScopeECtx(Nil, Some(ctx), Some(ast))

    // inherit all context from parent engine
//    this.ctx.root.overwrite(newCtx)

    val msgs = expandEMsgImpl(ast, ast.value.asInstanceOf[EMsg], true, level, newCtx)

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

  /** an assignment message */
  private def appendValsPas (a:DomAst, x:EMsgPas, attrs:List[PAS], appendToCtx:ECtx, kind:String=AstKinds.GENERATED) = {
    implicit val ctx = appendToCtx

    /** setting values */
    def setp (parent:Any, accessor:P, p:P)(implicit ctx:ECtx): P = {
      // reset - need to recalc accessors every time
      val av = accessor.copy(value=None).calculatedTypedValue
      val pcalc = p.calculatedP

      parent match {
        case c : ECtx => {
          // parent is context = just set value
          // accessor must be string
          if(av.contentType != WTypes.STRING)
            throw new DieselExprException(s"ctx. accessor must be string - we got: ${av.value}")
          c.put(pcalc.copy(name=av.asString))
        }
        case None => {
          throw new DieselExprException(s"parent not found")
        }
          // parent is a map/json object
        case Some(pa:RDOM.P) if pa.ttype == WTypes.JSON => {
          val pv = p.calculatedTypedValue
          val newv = PValue(
            pa.calculatedTypedValue.asJson + (av.asString -> p.calculatedTypedValue.value),
            WTypes.JSON
          )
          pa.value = Some(newv )

          // need to set dflt as well - NO WE DO NOT ANYMORE, use p.currentStringValue
          val newp = pa//.copy(dflt=newv.asString)
          // todo need to recurse on parent, not just hardcode ctx
          ctx.put(newp)
        }
      }
      pcalc
    }

    val calc = attrs.flatMap { pas =>
      if (pas.left.rest.isEmpty) {
        // classic p=e
        val calcp = P(pas.left.start, "").copy(expr = Some(pas.right)).calculatedP
        val calc = List(calcp)
        calc
      } else {
        // more complex left expr = right value
        a append DomAst(EInfo(pas.toString).withPos(x.pos), AstKinds.DEBUG)

        val p = pas.right.applyTyped("")

        val res = if (pas.left.rest.size > 1) { // [] accessors

        } else if (pas.left.start == "ctx") {
          setp(ctx, pas.left.rest.head, p)
        } else {
          val parent = pas.left.getp(pas.left.start)
//          a append DomAst(EInfo("parent: "+parent.mkString).withPos(x.pos), AstKinds.TRACE)
//          a append DomAst(EInfo("selector: "+pas.left.rest.head.calculatedTypedValue.toString).withPos(x.pos), AstKinds.TRACE)
          setp(parent, pas.left.rest.head, p)
        }
        a append DomAst(EInfo(pas.left.toStringCalc + " = " + res, p.calculatedTypedValue.asString).withPos(x.pos), AstKinds.DEBUG)
        Nil
      }
    }

      a appendAll calc.flatMap{p =>
        if(p.ttype == WTypes.EXCEPTION) {
          p.value.map {v=>
            val err = if(v.value.isInstanceOf[javax.script.ScriptException]) {
//             special handling of Script exceptions - no point showing stack trace
              new EError("ScriptException: " + p.dflt + v.asThrowable.getMessage)
            } else {
              new EError(p.dflt, v.asThrowable)
            }

            DomAst(err.withPos(x.pos), AstKinds.ERROR).withSpec(x) :: Nil
          } getOrElse {
            DomAst(EError(p.dflt) withPos (x.pos), AstKinds.ERROR).withSpec(x) :: Nil
          }
        } else {
          val newa = DomAst(EVal(p) withPos (x.pos), kind).withSpec(x)
          newa ::
          DomAst(EInfo(p.toString, p.calculatedTypedValue.asString).withPos(x.pos), AstKinds.DEBUG) :: Nil
        }
      }

      appendToCtx putAll calc
  }

  /** an assignment message */
  private def appendVals (a:DomAst, x:EMsg, attrs:Attrs, appendToCtx:ECtx, kind:String=AstKinds.GENERATED) = {
    a appendAll attrs.map{p =>
      if(p.ttype == WTypes.EXCEPTION) {
        p.value.map {v=>
          val err = if(v.value.isInstanceOf[javax.script.ScriptException]) {
            // special handling of Script exceptions - no point showing stack trace
            new EError("ScriptException: " + p.dflt + v.asThrowable.getMessage)
          } else {
            new EError(p.dflt, v.asThrowable)
          }

          DomAst(err.withPos(x.pos), AstKinds.ERROR).withSpec(x)
        } getOrElse {
          DomAst(EError(p.dflt) withPos (x.pos), AstKinds.ERROR).withSpec(x)
        }
      } else {
        DomAst(EVal(p) withPos (x.pos), kind).withSpec(x)
      }
    }

    appendToCtx putAll attrs

    if(appendToCtx.isInstanceOf[StaticECtx]) {
      a appendAll attrs.flatMap { p =>
        appendToCtx.asInstanceOf[StaticECtx].check(p).toList map {err=>
          DomAst(err.withPos(x.pos), AstKinds.ERROR).withSpec(x)
        }
      }
    }
  }

  /** reused - execute a rule */
  private def runRule (a:DomAst, in:EMsg, r:ERule, parentCtx:ECtx) = {
    var result : List[DomAst] = Nil
    implicit val ctx : ECtx = new StaticECtx(in.attrs, Some(parentCtx), Some(a))

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
          // todo should these go into the ctx or this.ctx
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
          Some(DomAst(x, AstKinds.NEXT).withSpec(r))
        }

        case x: ENextPas => {
          Some(DomAst(x, AstKinds.NEXT).withSpec(r))
        }

        case x @ _ => {
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
    else result

    result
  }

  /** if it's an internal engine message, execute it */
  private def expandEngineEMsg(a: DomAst, in: EMsg) : Boolean = {
    if(in.entity == "diesel" && in.met == "return") {
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
    } else if(in.entity == "diesel" && in.met == DieselMsg.ENGINE.VALS) {
      // expand all spec vals
      dom.moreElements.collect {
        case v:EVal => {
          evAppChildren(a, DomAst(v, AstKinds.TRACE))
          this.ctx.put(v.p)
        }
      }

      true
    } else if(in.entity == DieselMsg.SCOPE.ENTITY && in.met == "push") {
      this.ctx = new ScopeECtx(Nil, Some(this.ctx), Some(a))
      true
    } else if(in.entity == DieselMsg.SCOPE.ENTITY && in.met == "pop" && this.ctx.isInstanceOf[ScopeECtx]) {
      this.ctx = this.ctx.base.get
      true
    } else if(in.entity == "diesel.engine" && in.met == "debug") {
      val s = this.settings.toJson
      val c = this.ctx.toString
      val e = this.toString
      evAppChildren(a, DomAst(EInfo("settings", js.tojsons(s)), AstKinds.DEBUG))
      evAppChildren(a, DomAst(EInfo("ctx", c), AstKinds.DEBUG))
      evAppChildren(a, DomAst(EInfo("engine", e), AstKinds.DEBUG))
      true
    } else {
      false
    }
  }

  /** expand a single message */
  private def expandEMsgImpl(a: DomAst, in: EMsg, recurse: Boolean, level: Int, parentCtx:ECtx) : List[DomAst] = {
    var newNodes : List[DomAst] = Nil // nodes generated this call collect here

//    implicit var ctx = new StaticECtx(in.attrs, Some(parentCtx), Some(a))
    implicit var ctx = parentCtx

    // if the message attrs were expressions, calculate their values
    val n: EMsg = in.copy(
      attrs = in.attrs.map { p =>
        p.calculatedP // only calculate if not already calculated =likely in a different context=
      }
    )

    ctx = new StaticECtx(n.attrs, Some(parentCtx), Some(a))

    // 1. engine message?
    var mocked = expandEngineEMsg(a, n)

    // 1. if not, look for mocks
    if (settings.mockMode) {
      (root.collect {
        // mocks from story AST
        case d@DomAst(m: EMock, _, _, _) if m.rule.e.test(n) && a.children.isEmpty => m
      } ::: dom.moreElements.toList).collect {
        // todo perf optimize moreelements.toList above
        // plus mocks from spec dom
        case m: EMock if m.rule.e.test(n) && a.children.isEmpty => {
          mocked = true

          // run the mock
          newNodes = newNodes ::: runRule(a, n, m.rule, ctx)
        }
      }
    }

    // 2. rules
    var ruled = false
    // no engine messages fit, so let's find rules
    // I run rules even if mocks fit - mocking mocks only going out, not decomposing
    // todo - WHY? only some systems may be mocked ???
    if ((true || !mocked) && !settings.simMode) {
      val matchingRules = rules.filter(_.e.test(n))
      val exclusives = HashMap[String, ERule]()

      matchingRules.map { r =>
        // each matching rule
        ruled = true

        //filter out exclusives
        val exKey = r.e.cls + "." + r.e.met
        if(exclusives.contains(exKey)) {
          newNodes = newNodes :::
              DomAst(EInfo("rule excluded", exKey).withPos(r.pos), AstKinds.TRACE) ::
              Nil
        } else {
          if(r.arch.contains("exclusive")) {
            exclusives.put (r.e.cls + "." + r.e.met, r) // we do exclusive per pattern
          }

          newNodes = newNodes ::: runRule(a, n, r, ctx)
        }
      }
    }

    // 3. executors

    // no mocks, let's try executing it
    // todo WHY - I run snaks even if rules fit - but not if mocked
    // todo now I run only if nothing else fits
    if (!mocked && !ruled && !settings.simMode) {
      // todo can I just take the first executor that works?
      Executors.withAll(_.filter { x =>
        // todo inconsistency: I am running rules if no mocks fit, so I should also run
        //  any executor ??? or only the isMocks???
        (true /*!settings.mockMode || x.isMock*/) && x.test(n)
      }.map { r =>
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
            List(DomAst(new EError("Exception:", e), AstKinds.ERROR))
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
        if(!ms.startsWith("diesel."))
          evAppChildren(a, DomAst(
            EWarning(
              "No rules, mocks or executors match for " + in.toString,
              "Review your engine configuration (blender, mocks, drafts, tags), " +
                "spelling of messages or rule clauses / pattern matches"),
            AstKinds.DEBUG
          ))
      }
    }

    // 4. sketch

    // last ditch attempt, in sketch mode: if no mocks or rules, run the expects
    if (!mocked && settings.sketchMode) {
      // sketch messages
      (collectValues {
        case x: ExpectM if x.when.exists(_.test(n)) => x
      }).map { e =>
        mocked = true

        val spec = dom.moreElements.collect {
          case x: EMsg if x.entity == e.m.cls && x.met == e.m.met => x
        }.headOption

        val newctx = new StaticECtx(n.attrs, Some(ctx), Some(a))
        val news = e.sketch(None)(newctx).map(x => DomAst(x, AstKinds.SKETCHED).withSpec(e))
        newNodes = newNodes ::: news
      }

      // sketch values
      (collectValues {
        case x: ExpectV if x.when.exists(_.test(n)) => x
      }).map { e =>
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

  /** expand a single message - called by engine, results in engine processing */
  private def expandEMsg(a: DomAst, in: EMsg, recurse: Boolean, level: Int, parentCtx:ECtx) : List[DEMsg] = {
    val newNodes = expandEMsgImpl(a, in, recurse, level, parentCtx)

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

      razie.Log.log("while decompose em()", t)

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

        razie.Log.log("while decompose ev()", t)

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

  /** main processing of next - called from actor in async and in thread when sync/decompose */
  def processDEMsg (m:DEMsg) = {
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
        val msgs = done(a, l)
        checkState()
        later(msgs)
      }
    }
  }

  /** extract the resulting values from this engine */
  def extractValues (e:String, a:String) = {
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

  /** extract the resulting value from this engine
    * extract one value - try:
    * 1. defined response oattrs
    * 2. last valuep - the last message produced in the flow
    * 3. payload
    * */
  def extractFinalValue (e:String, a:String) = {
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
      .orElse(valuesp.find(_.name == "payload"))

    resp
  }

  def finalContext (e:String, a:String) = {
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

trait InfoNode // just a marker for useless info nodes

