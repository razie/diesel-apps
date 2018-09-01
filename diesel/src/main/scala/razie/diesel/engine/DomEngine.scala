/**
 *  ____    __    ____  ____  ____  ___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

import org.bson.types.ObjectId
import org.joda.time.DateTime
import razie.clog
import razie.diesel.dom.{DomState, RDomain, _}
import razie.diesel.engine.RDExt._
import razie.diesel.ext.{BFlowExpr, FlowExpr, MsgExpr, SeqExpr, _}
import razie.diesel.utils.DomCollector
import razie.tconf.DSpec
import razie.wiki.Enc

import scala.Option.option2Iterable
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.Try

/** a trace */
case class DieselTrace (
  root:DomAst,
  node:String,        // actual server node
  engineId:String,      // engine Id
  app:String,         // application/system id twitter:pc:v5
  details:String="",
  parentNodeId:Option[String]=None ) extends CanHtml with InfoNode {

  def toj: Map[String, Any] =
    Map(
      "class" -> "DieselTrace",
      "ver" -> "v1",
      "node" -> node,
      "engineId" -> engineId,
      "app" -> app,
      "details" -> details,
      "id" -> root.id,
      "parentNodeId" -> parentNodeId.mkString,
      "root" -> root.toj
    )

  def toJson = toj

  override def toHtml = span("trace::", "primary") + s"$details (node=$node, engine=$engineId, app=$app) :: " + root.toHtml

  override def toString = toHtml
}


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


/** the engine: one flow = one engine = one actor */
class DomEngine(
  val dom: RDomain,
  val root: DomAst,
  val settings: DomEngineSettings,
  val pages : List[DSpec],
  val id:String = new ObjectId().toString) {

  var status = DomState.INIT
  var synchronous = false

  val maxLevels = 25

  // setup the context for this eval
  implicit var ctx : ECtx = new DomEngECtx(settings)
    .withEngine(this)
    .withDomain(dom)
    .withSpecs(pages)
    .withCredentials(settings.userId)
    .withHostname(settings.node)

  def href (format:String="") = s"/diesel/engine/view/$id?format=$format"

  def failedTestCount = (root.collect {
    case d@DomAst(n: TestResult, _, _, _) if n.value.startsWith("fail") => n
    case d@DomAst(n: EError, _, _, _) => n
  }).size

  def successTestCount = (root.collect {
    case d@DomAst(n: TestResult, _, _, _) if n.value.startsWith("ok") => n
  }).size

  /** collect generated values */
  def resultingValues() = root.collect {
    // todo see in Api.irunDom, trying to match them to the message sent in...
    case d@DomAst(EVal(p), /*AstKinds.GENERATED*/ _, _, _) /*if oattrs.isEmpty || oattrs.find(_.name == p.name).isDefined */ => (p.name, p.dflt)
  }

  /** collect the last generated value OR empty string */
  def resultingValue = root.collect {
    case d@DomAst(EVal(p), /*AstKinds.GENERATED*/ _, _, _) /*if oattrs.isEmpty || oattrs.find(_.name == p.name).isDefined */ => (p.name, p.dflt)
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

      case DEventExpNode(parentId, children, _) =>
        n(parentId).children appendAll children

      case DEventNodeStatus(parentId, status, _) =>
        n(parentId).status = status

      case DADepyEv(pId, dId, _) =>
        depys.append(DADepy(n(pId),n(dId)))
    }
  }

  //==========================

  // todo it's faster here where i have the node handles than looking it up by id above...

  def evAppChildren (parent:DomAst, children:DomAst) : Unit =
    evAppChildren(parent, List(children))

  def evAppChildren (parent:DomAst, children:List[DomAst]) : Unit = {
//    parent.children appendAll children
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

  /** spawn new engine */
  def spawn (nodes:List[DomAst]) = {
    val newRoot = DomAst("root", AstKinds.ROOT).withDetails("(spawned)")
    evAppChildren(newRoot, nodes)
    val engine = DieselAppContext.mkEngine(dom, newRoot, settings, pages)
    engine.ctx.root._hostname = ctx.root._hostname
    engine
  }

  /** process a list of continuations */
  def later (m:List[DEMsg]) = {
    if(synchronous) {
      m.map(processDEMsg)
    } else
      m.map(m=>DieselAppContext.router.map(_ ! m))
  }

  /** stop me now */
  def stopNow = {
      status = DomState.DONE
      clog << "DomEng "+id+" stopNow"
      DomCollector.collectAst ("engine", id, this)
      finishP.success(this)
      DieselAppContext.stopActor(id)
  }

  def collectValues[T] (f: PartialFunction[Any, T]) : List[T] =
    root.collect {
      case v if(f.isDefinedAt(v.value)) => f(v.value)
    }

  def findParent(node: DomAst): Option[DomAst] =
    root.collect {
      case a if a.children.exists(_.id == node.id) => a
    }.headOption

  // completed a node - udpate stat
  def done(a: DomAst, level:Int=1): List[DEMsg] = {
    evChangeStatus(a, DomState.DONE)

    clog << "DomEng "+id+("  " * level)+" done " + a.value

    // parent status update

    val x =
      checkState(findParent(a).getOrElse(root)) :::
      depys.toList.filter(_.prereq.id == a.id).flatMap(d=>
        root.find(d.depy.id).toList
      ).filter { n =>
        val prereq = depys.filter(x => x.depy.id == n.id && DomState.inProgress(x.prereq.status))
        prereq.isEmpty && n.status == DomState.DEPENDENT
      }.map { n =>
        evChangeStatus(n, DomState.LATER)
        DEReq(id, n, true, level/* + 1*/) // don't increase level for depys - they're not "deep" but equals
        // todo i think depys are not even equals - they would have their own level...?
      }
    x
  }

  /** dependency between two nodes */
  case class DADepy (prereq:DomAst, depy:DomAst)
  case class DADepyEv (prereq:String, depy:String, dtm:DateTime=DateTime.now) extends DEvent

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
    * @param results
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

    if(a.value.isInstanceOf[EMsg]) {
      implicit val ctx = new StaticECtx(a.value.asInstanceOf[EMsg].attrs, Some(this.ctx), Some(a))
      flows.filter(_.e.test(a.value.asInstanceOf[EMsg])).map { f =>
        res = rec(f.ex)
      }
    }

    // add explicit depys
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
    evChangeStatus(a, DomState.STARTED)

    clog << "DomEng "+id+("  " * level)+" expand " + a.value
    var msgs = Try {
      expand(a, recurse, level + 1)
    }.recover {
      case t:Throwable => {
        razie.Log.log("wile decompose()", t)
        val err = DEError(this.id, t.toString)
        val ast = DomAst(new EError("Exception", t)).withStatus(DomState.DONE)
        evAppChildren(a, ast)
        err :: done(ast, level+1)
      }
    }.get

    if(msgs.isEmpty)
      msgs = msgs ::: done(a, level)

    checkState()
    later(msgs)
  }

  /** process a reply with results
    *
    * will decompose this node into children and spawn async those that need it
    *
    * @param a the node that got this reply
    * @param recurse
    * @param level
    * @param results
    * @return
    */
  def rep(a: DomAst, recurse: Boolean = true, level: Int, results:List[DomAst]): List[DEMsg] = {
    var msgs : List[DEMsg] = Nil // continuations from this cycle

    evAppChildren(a, results)

    if (recurse)
      msgs = msgs ::: findFront(a, results).map { n =>
//      req(n, recurse, level + 1)
      evChangeStatus(n, DomState.LATER)
      DEReq(id, n, recurse, level+1) // increase level for children
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
    } else {
      if(node.status != DomState.DONE)
        // call only when transitioning to DONE
        result = done(node) //node.status = DONE
    }

    // when all done, the entire engine is done

    if(root.status == DomState.DONE && status != DomState.DONE) {
      status = DomState.DONE
      clog << "DomEng "+id+" finish"
      DomCollector.collectAst ("engine", id, this)
      finishP.success(this)
      DieselAppContext.refMap.get(id).map(_ ! DEStop) // stop the actor and remove engine
    }

    result
  }

  /** add built-in triggers */
  private def prepRoot(l:ListBuffer[DomAst]) : ListBuffer[DomAst] = {
    l.prepend(DomAst(EMsg("diesel", "before"), AstKinds.BUILTIN))
    l.append(DomAst(EMsg("diesel", "after"), AstKinds.BUILTIN))
    l
  }

  /** process only the tests - synchronously */
  def processTests = {
    Future {
      prepRoot(root.children.filter(_.kind == "test")).foreach(expand(_, true, 1))
      this
    }
  }

  // waitingo for flow to complete
  val finishP = Promise[DomEngine]()
  val finishF = finishP.future

  def process : Future[DomEngine] = {
    if(root.status != DomState.STARTED) {

      clog << "DomEng " + id + " process"
      root.status = DomState.STARTED

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
      a.children append DomAst(TestResult("fail: Max-Level!", "You have a recursive rule generating this branch..."), "error")
      return Nil;
    }

    a.value match {

      case stop : EEngStop => {
        // start new engine/process
        // todo should find settings for target service  ?
        a.children append DomAst(EInfo(s"""Stopping engine <a href="/diesel/engine/view/${this.id}">${this.id}</a>"""))//.withPos((m.get.pos)))
        stopNow
      }

      case next@ENext(m, "==>", cond, _) => {
        // forking new engine / async branch
        implicit val ctx = new StaticECtx(next.parent.map(_.attrs).getOrElse(Nil), Some(this.ctx), Some(a))
        // start new engine/process
        // todo should find settings for target service  ?
        val eng = spawn(List(DomAst(next.evaluateMsg)))
        eng.process // start it up in the background
        a.children append DomAst(EInfo(s"""Spawn engine <a href="/diesel/engine/view/${eng.id}">${eng.id}</a>"""))//.withPos((m.get.pos)))
        // no need to return anything - children have been decomposed
      }

      case n1@ENext(m, "=>", cond, _) => {
        // message executed later
        implicit val ctx = new StaticECtx(n1.parent.map(_.attrs).getOrElse(Nil), Some(this.ctx), Some(a))

        if(n1.test()) {
          val newnode = DomAst(n1.evaluateMsg, AstKinds.GENERATED)
//          a.children append newnode
          msgs = rep(a, recurse, level, List(newnode))
        }
        // message will be evaluate() later
      }

      case x: EMsg if x.entity == "" && x.met == "" => {
        // just expressions, generate values from each attribute
        a.children appendAll x.attrs.map{p =>
          if(p.ttype == WTypes.EXCEPTION) {
            p.value.map {v=>
              DomAst(new EError(p.dflt, v.value.asInstanceOf[Throwable]) withPos(x.pos), AstKinds.GENERATED).withSpec(x)
            } getOrElse
              DomAst(EError(p.dflt) withPos(x.pos), AstKinds.GENERATED).withSpec(x)
          } else
            DomAst(EVal(p) withPos(x.pos), AstKinds.GENERATED).withSpec(x)
        }
        ctx putAll x.attrs
      }

      case in: EMsg => {
        msgs = expandEMsg(a, in, recurse, level) ::: msgs
      }

      case n: EVal if !AstKinds.isGenerated(a.kind) => {
        // $val defined in scope
        a.children append DomAst(EVal(n.p).withPos(n.pos), AstKinds.MOCKED)
        ctx.put(n.p)
      }

      case e: ExpectM => expandExpectM(a, e)

      case e: ExpectV => expandExpectV(a, e)

      case e: ExpectAssert => expandExpectAssert(a, e)

      case e:InfoNode =>  // ignore
      case e:EVal =>  // ignore

      // todo should execute
      case s: EApplicable =>  {
        clog << "EXEC KNOWN: " + s.toString
        a.children append DomAst(EError("Can't EApplicable KNOWN - " + s.getClass.getSimpleName, s.toString), AstKinds.GENERATED)
      }

      case s@_ => {
        clog << "NOT KNOWN: " + s.toString
        a.children append DomAst(
          EWarning("NODE NOT KNOWN - " + s.getClass.getSimpleName, s.toString),
          AstKinds.GENERATED)
      }
    }
    msgs
  }

  /** reused - execute a rule */
  private def runRule (a:DomAst, in:EMsg, r:ERule) = {
    var result : List[DomAst] = Nil
    implicit val ctx = new StaticECtx(in.attrs, Some(this.ctx), Some(a))

    // generate each gen/map
    r.i.map { ri =>
      // find the spec of the generated message, to ref
      val spec = dom.moreElements.collect {
        case x: EMsg if x.entity == ri.cls && x.met == ri.met => x
      }.headOption

      // defer evaluation if multiple messages generated - if just one message, evaluate values now
      var newMsgs = ri.apply(in, spec, r.pos, r.i.size > 1).collect {
        // hack: if only values then collect resulting values and dump message
        // only for size 1 because otherwise they may be in a sequence of messages and their value
        // may depend on previous messages and can't be evaluated now
        //
        case x: EMsg if x.entity == "" && x.met == "" && r.i.size == 1 => {
          a.children appendAll x.attrs.map(p => DomAst(EVal(p).withPos(x.pos), AstKinds.GENERATED).withSpec(r))
          ctx putAll x.attrs
          None
        }
        case x: EMsg if x.entity == "" && x.met == "" && r.i.size > 1 => {
          // can't execute now, but later
          //            a.children appendAll x.attrs.map(x => DomAst(EVal(x), AstKinds.GENERATED).withSpec(r))
          //            ctx putAll x.attrs
          //            None
          Some(DomAst(ENext(x, "=>", None, true), AstKinds.NEXT).withSpec(r))
        }
        // else collect message
        case x: EMsg => {
          if(r.i.size > 1)
            Some(DomAst(ENext(x, "=>"), AstKinds.NEXT).withSpec(r))
          else
            Some(DomAst(x, AstKinds.GENERATED).withSpec(r))
        }
        case x: ENext => {
          Some(DomAst(x, AstKinds.NEXT).withSpec(r))
        }
        case x @ _ => {
          Some(DomAst(x, AstKinds.GENERATED).withSpec(r))
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
    if(in.entity == "diesel.scope" && in.met == "push") {
      this.ctx = new ScopeECtx(Nil, Some(this.ctx), Some(a))
      true
    } else if(in.entity == "diesel.scope" && in.met == "pop") {
      this.ctx = this.ctx.base.get
      true
    } else {
      false
    }
  }

  /** expand a single message */
  private def expandEMsg(a: DomAst, in: EMsg, recurse: Boolean, level: Int) : List[DEMsg] = {
    var newNodes : List[DomAst] = Nil // nodes generated this call collect here

    implicit val ctx = new StaticECtx(in.attrs, Some(this.ctx), Some(a))

    // if the message attrs were expressions, calculate their values
    val n: EMsg = in.copy(
      attrs = in.attrs.map { p =>
        p.calculatedP // only calculate if not already calculated =likely in a different context=
      }
    )

    // 1. look for mocks
    var mocked = expandEngineEMsg(a, n)

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
          newNodes = newNodes ::: runRule(a, n, m.rule)
        }
      }
    }

    // 2. rules

    var ruled = false
    // no mocks fit, so let's find rules
    // I run rules even if mocks fit - mocking mocks only going out, not decomposing
    // todo - WHY ?
    if (true || !mocked) {
      rules.filter(_.e.test(n)).map { r =>
        // each matching rule
        ruled = true

        newNodes = newNodes ::: runRule(a, n, r)
      }
    }

    // 3. executors

    // no mocks, let's try executing it
    // todo WHY - I run snaks even if rules fit - but not if mocked
    // todo now I run only if nothing else fits
    if (!mocked && !ruled) {
      Executors.all.filter { x =>
        // todo inconsistency: I am running rules if no mocks fit, so I should also run any executor ??? or only the isMocks???
        (true /*!settings.mockMode || x.isMock*/) && x.test(n)
      }.map { r =>
        mocked = true

        val news = try {
          implicit val ctx = new StaticECtx(n.attrs, Some(this.ctx), Some(a))

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
//            case l@List => e
            case e@_ => e
          }.map(x =>
            DomAst(x,
              (if (x.isInstanceOf[DieselTrace]) AstKinds.SUBTRACE
              else AstKinds.GENERATED)
            ).withSpec(r)
          )
        } catch {
          case e: Throwable =>
            razie.Log.alarmThis("wtf", e)
            List(DomAst(new EError("Exception:", e), AstKinds.GENERATED))
        }

        newNodes = newNodes ::: news
      }

      if(newNodes.isEmpty) {
        clog << "NO matches: " + in.toString
        a.children append DomAst(
          EWarning(
            "No rules, mocks or executors match for " + in.toString,
            "Review your engine configuration (blender, mocks, drafts, tags), " +
              "spelling of messages or rule clauses / pattern matches"),
          AstKinds.GENERATED)
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
          a.children append n
        }
      }
    }

    // analyze the new messages and return
    rep(a, recurse, level, newNodes)
  }

  /** test epected message */
  private def expandExpectM(a: DomAst, e: ExpectM) : Unit = {
    val cole = new MatchCollector()

    Try {

    val targets = e.target.map(List(_)).getOrElse(if (e.when.isDefined) {
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
            a.children append DomAst(TestResult("ok").withPos(e.pos), "test").withSpec(e)
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
        val s = c.diffs.values.map(_._2).toList.map(x => s"""<span style="color:red">$x</span>""").mkString(",")
        d.moreDetails = d.moreDetails + label("expected", "danger") + " " + s
      }

      // did some rules succeed?
      if (a.children.isEmpty) {
        // oops - add test failure
        a.children append DomAst(
          TestResult(
            "fail",
            "",
            label("found", "warning") + " " +
            cole.highestMatching.map(_.diffs.values.map(_._1).toList.map(x => s"""<span style="color:red">$x</span>""").mkString(",")).mkString
          ).withPos(e.pos),
          AstKinds.TEST
        ).withSpec(e)
      }
    }

  }.recover {
    case t:Throwable => {

      razie.Log.log("while decompose em()", t)

      // oops - add test failure
      a.children append DomAst(
        TestResult(
          "fail",
          "",
          cole.toHtml
        ),
        AstKinds.TEST
      ).withSpec(e)

      a.children append DomAst(new EError("Exception", t)).withStatus(DomState.DONE)
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
        val vvals = n.collect {
          case d@DomAst(n: EVal, k, _, _) if AstKinds.isGenerated(k) => d
        }

        // include the message's values in its context
        val newctx = new StaticECtx(n.value.asInstanceOf[EMsg].attrs, Some(ctx), Some(n))

        val values = vvals.map(_.value.asInstanceOf[EVal].p)

        if (vvals.size > 0 &&
          !e.applicable(values)(newctx)) {
          a.children append DomAst(
            TestResult("n/a").withPos(e.pos),
            AstKinds.TEST
          ).withSpec(e)
        } else if (vvals.size > 0 &&
          e.test(values, Some(cole), vvals)(newctx)) {

          a.children append DomAst(
            TestResult("ok").withPos(e.pos),
            AstKinds.TEST
          ).withSpec(e)
        } else
        //if no rules succeeded and there were vals, collect the misses
          values.map(v=> cole.missed(v.name))
      }

      // test each generated value
      //      if (e.test(vals.map(_.value.asInstanceOf[EVal].p), Some(cole), vals))
      //        a.children append DomAst(TestResult("ok"), AstKinds.TEST).withSpec(e)

      cole.done

      // did some rules succeed?
      if (a.children.isEmpty) {
        // oops - add test failure
        a.children append DomAst(
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
        val s = c.diffs.values.map(_._2).toList.map(x => s"""<span style="color:red">$x</span>""").mkString(",")
        d.moreDetails = d.moreDetails + label("expected", "danger") + " " + htmlValue(s)
      }

      // did some rules succeed?
      if (a.children.isEmpty) {
        // oops - add test failure
        a.children append DomAst(
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
        a.children append DomAst(
          TestResult(
            "fail",
            "",
            cole.toHtml
          ),
          AstKinds.TEST
        ).withSpec(e)

        a.children append DomAst(new EError("Exception", t)).withStatus(DomState.DONE)
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
          a.children append DomAst(
            TestResult("ok").withPos(e.pos),
            AstKinds.TEST
          ).withSpec(e)
        else
          //if no rules succeeded and there were vals, collect the misses
          values.map(v=> cole.missed(v.name))
    }

      // test each generated value
//      if (e.test(vals.map(_.value.asInstanceOf[EVal].p), Some(cole), vals))
//        a.children append DomAst(TestResult("ok"), AstKinds.TEST).withSpec(e)

      cole.done

      // did some rules succeed?
      if (a.children.isEmpty) {
        // oops - add test failure
        a.children append DomAst(
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
        a.children append DomAst(
          TestResult(
            "fail",
            label("found", "warning") + " " +
              cole.highestMatching.map(_.diffs.values.map(_._1).toList.map(x => s"""<span style="color:red">$x</span>""").mkString(",")).mkString
          ).withPos(e.pos),
          AstKinds.TEST
        ).withSpec(e)
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
    }
  }

  def extractValues (e:String, a:String) = {
    // find the spec and check its result
    // then find the resulting value.. if not, then json
    val oattrs = dom.moreElements.collect {
      //      case n:EMsg if n.entity == e && n.met == a => n
      case n: EMsg if n.entity == e && n.met == a => n
    }.headOption.toList.flatMap(_.ret)

    //    if (oattrs.isEmpty) {
    //      errors append s"Can't find the spec for $msg"
    //    }

    // collect values
    val values = root.collect {
      case d@DomAst(EVal(p), /*AstKinds.GENERATED*/ _, _, _) if oattrs.isEmpty || oattrs.find(_.name == p.name).isDefined => p
    }

    values
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
