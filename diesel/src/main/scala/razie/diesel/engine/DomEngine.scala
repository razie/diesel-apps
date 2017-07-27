/**
 *  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

import akka.actor.{ActorRef, Props}
import razie.diesel.dom.DomState
import RDExt._
import razie.diesel.ext.{BFlowExpr, FlowExpr, MsgExpr, SeqExpr}
import org.bson.types.ObjectId
import org.joda.time.DateTime
import play.libs.Akka
import razie.clog
import razie.diesel.dom.{RDomain, _}
import razie.diesel.ext._

import scala.Option.option2Iterable
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.Try



/** an application static - engine factory and cache */
object DieselAppContext {
  private var appCtx : Option[DieselAppContext] = None
  var engMap = new mutable.HashMap[String,DomEngine]()
  var refMap = new mutable.HashMap[String,ActorRef]()
  var router : Option[ActorRef] = None

  /** when in a cluster, you need to set this on startup... */
  var localNode = "localhost"

  /** initialize the engine cache and actor infrastructure */
  def init (node:String="", app:String="") = {
    if(appCtx.isEmpty) appCtx = Some(new DieselAppContext(
      if(node.length > 0) node else localNode,
      if(app.length > 0) app else "default"
    ))

    val p = Props(new DomEngineRouter())
    val a = Akka.system.actorOf(p)
    router = Some(a)
    a ! DEInit

    appCtx.get
  }

  /** the static version - delegates to factory */
  def mkEngine(dom: RDomain, root: DomAst, settings: DomEngineSettings, pages : List[DSpec]) = {
    val eng = ctx.mkEngine(dom, root, settings, pages)
    val p = Props(new DomEngineActor(eng))
    val a = Akka.system.actorOf(p, name = "engine-"+eng.id)
    DieselAppContext.engMap.put(eng.id, eng)
    DieselAppContext.refMap.put(eng.id, a)
    a ! DEInit
    eng
  }

  def engines = engMap.values.toList

  def stop = {
  }

  def ctx = appCtx.getOrElse(init())
}

/** a diesel app context
  *
  * todo properly injecting these */
class DieselAppContext (node:String, app:String) {
  /** make an engine instance for the given AST root */
  def mkEngine(dom: RDomain, root: DomAst, settings: DomEngineSettings, pages : List[DSpec]) =
    new DomEngine(dom, root, settings, pages)
}

/** a trace */
case class DieselTrace(
  root:DomAst,
  node:String,        // actual server node
  engineId:String,      // engine Id
  app:String,         // application/system id twitter:pc:v5
  details:String="",
  parentNodeId:Option[String]=None ) extends CanHtml {

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


/** base trait for events */
trait DEvent {
  def dtm : DateTime
}

/** node expanded */
case class DEventExpNode(nodeId:String, children:List[DomAst], dtm:DateTime=DateTime.now) extends DEvent with CanHtml {
  override def toHtml = s"EvExNode: $nodeId, ${children.map(_.toString).mkString}"
}

/** an engine */
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
  implicit val ctx = new DomEngECtx(settings)
    .withEngine(this)
    .withDomain(dom)
    .withSpecs(pages)
    .withCredentials(settings.userId)

  def href (format:String="") = s"/diesel/engine/view/$id?format=$format"

  def failedTestCount = (root.collect {
    case d@DomAst(n: TestResult, _, _, _) if n.value.startsWith("fail") => n
  }).size

  def successTestCount = (root.collect {
    case d@DomAst(n: TestResult, _, _, _) if n.value.startsWith("ok") => n
  }).size

  /** collect generated values */
  def resultingValues = root.collect {
    // todo see in Api.irunDom, trying to match them to the message sent in...
    case d@DomAst(EVal(p), /*"generated"*/ _, _, _) /*if oattrs.isEmpty || oattrs.find(_.name == p.name).isDefined */ => (p.name, p.dflt)
  }

  /** collect generated values */
  def resultingValue = root.collect {
    case d@DomAst(EVal(p), /*"generated"*/ _, _, _) /*if oattrs.isEmpty || oattrs.find(_.name == p.name).isDefined */ => (p.name, p.dflt)
  }

  val rules = dom.moreElements.collect {
    case e:ERule => e
  }

  val flows = dom.moreElements.collect {
    case e:EFlow => e
  }

  //========================== DDD

  val events : ListBuffer[DEvent] = new ListBuffer[DEvent]()

  def addEvent(e:DEvent*) = {
    events.append(e:_*)
  }

  def appChildren (parent:DomAst, children:DomAst) : Unit =
    appChildren(parent, List(children))

  def appChildren (parent:DomAst, children:List[DomAst]) : Unit = {
    parent.children appendAll children
    addEvent(DEventExpNode(parent.id, children))
  }

  /** spawn new engine */
  def spawn (nodes:List[DomAst]) = {
    val newRoot = DomAst("root", AstKinds.ROOT).withDetails("(spawned)")
    appChildren(newRoot, nodes)
    val engine = DieselAppContext.mkEngine(dom, newRoot, settings, pages)
    engine.ctx._hostname = ctx._hostname
    engine
  }

  /** process a list of continuations */
  def later (m:List[DEMsg]) = {
    if(synchronous) {
      m.map(processDEMsg)
    } else
      m.map(m=>DieselAppContext.router.map(_ ! m))
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
    a.status = DomState.DONE

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
        n.status = DomState.LATER
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
      if(p.nonEmpty) d.status = DomState.DEPENDENT
      p.map{p=>
        addEvent(DADepyEv(p.id,d.id))
        depys.append(DADepy(p,d))
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
    a.status = DomState.STARTED

    clog << "DomEng "+id+("  " * level)+" expand " + a.value
    var msgs = Try {
      expand(a, recurse, level + 1)
    }.recover {
      case t:Throwable => {
        razie.Log.log("wile decompose()", t)
        val err = DEError(this.id, t.toString)
        val ast = DomAst(EError(t.getMessage, t.toString)).withStatus(DomState.DONE)
        appChildren(a, ast)
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

    a.children appendAll results
    if (recurse)
      msgs = msgs ::: findFront(a, results).map { n =>
//      req(n, recurse, level + 1)
      n.status = DomState.LATER
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
      node.status = DomState.STARTED
    } else {
      if(node.status != DomState.DONE)
        // call only when transitioning to DONE
        result = done(node) //node.status = DONE
    }

    // when all done, the entire engine is done

    if(root.status == DomState.DONE && status != DomState.DONE) {
      status = DomState.DONE
      clog << "DomEng "+id+" finish"
      finishP.success(this)
      DieselAppContext.refMap.get(id).map(_ ! DEStop) // stop the actor and remove engine
    }

    result
  }

  /** process only the tests - synchronously */
  def processTests = {
    Future {
      root.children.filter(_.kind == "test").foreach(expand(_, true, 1))
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
      val msgs = findFront(root, root.children.toList).toList.map { x =>
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

      case next@ENext(m, "==>", cond) => {
        // start new engine/process
        // todo should find settings for target service  ?
        val eng = spawn(List(DomAst(m)))
        eng.process // start it up in the background
        a.children append DomAst(EInfo(s"""Spawn engine <a href="/diesel/engine/view/${eng.id}">${eng.id}</a>"""))//.withPos((m.get.pos)))
        // no need to return anything - children have been decomposed
      }

      case n1@ENext(m, "=>", cond) => {
        implicit val ctx = new StaticECtx(m.attrs, Some(this.ctx), Some(a))
        if(n1.test()) {
          val newnode = DomAst(m, AstKinds.GENERATED)
//          a.children append newnode
          msgs = rep(a, recurse, level, List(newnode))
        }
      }

      case x: EMsg if x.entity == "" && x.met == "" => {
        a.children appendAll x.attrs.map(p => DomAst(EVal(p)withPos(x.pos), AstKinds.GENERATED).withSpec(x))
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

      case e:InfoNode =>  // ignore
      case e:EVal =>  // ignore

      // todo should execute
      case s: EApplicable =>  {
        clog << "EXEC KNOWN: " + s.toString
        a.children append DomAst(EError("Can't EApplicable KNOWN - " + s.getClass.getSimpleName, s.toString), AstKinds.GENERATED)
      }

      case s@_ => {
        clog << "NOT KNOWN: " + s.toString
        a.children append DomAst(EWarning("NODE NOT KNOWN - " + s.getClass.getSimpleName, s.toString), AstKinds.GENERATED)
      }
    }
    msgs
  }

  /** reused */
  private def runRule (a:DomAst, in:EMsg, r:ERule) = {
    var result : List[DomAst] = Nil
    implicit val ctx = new StaticECtx(in.attrs, Some(this.ctx), Some(a))

    // generate each gen/map
    r.i.map { ri =>
      // find the spec of the generated message, to ref
      val spec = dom.moreElements.collect {
        case x: EMsg if x.entity == ri.cls && x.met == ri.met => x
      }.headOption

      var newMsgs = ri.apply(in, spec, r.pos).collect {
        // hack: if only values then collect resulting values and dump message
        // only for size 1 because otherwise they may be in a sequence of messages and their value
        // may depend on previous messages and can't be evaluated now
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
          Some(DomAst(ENext(x, "=>"), AstKinds.NEXT).withSpec(r))
        }
        // else collect message
        case x: EMsg => {
          Some(DomAst(x, AstKinds.GENERATED).withSpec(r))
        }
        case x: ENext => {
          Some(DomAst(x, AstKinds.GENERATED).withSpec(r))
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

  private def expandEMsg(a: DomAst, in: EMsg, recurse: Boolean, level: Int) = {
    var newNodes : List[DomAst] = Nil // nodes generated this call collect here

    implicit val ctx = new StaticECtx(in.attrs, Some(this.ctx), Some(a))

    // if the message attrs were expressions, calculate their values
    val n: EMsg = in.copy(
      attrs = in.attrs.map {p=>
        p.copy(
          // only if not already calculated =likely in a different context=
          dflt = if(p.dflt.nonEmpty) p.dflt else p.expr.map(_.apply("").toString).getOrElse(p.dflt),
          ttype = if(p.ttype.nonEmpty) p.ttype else p.expr match {
            case Some(CExpr(_, WTypes.STRING)) => WTypes.STRING
            case Some(CExpr(_, WTypes.NUMBER)) => WTypes.NUMBER
            case _ => p.ttype
          }
        )
      }
    )

    // 1. look for mocks
    var mocked = false

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
    if (true || !mocked) {
      rules.filter(_.e.test(n)).map { r =>
        // each matching rule
        ruled = true

        newNodes = newNodes ::: runRule(a, n, r)
      }
    }

    // 3. executors

    // no mocks, let's try executing it
    // I run snaks even if rules fit - but not if mocked
    if (!mocked) {
      Executors.all.filter { x =>
        (!settings.mockMode || x.isMock) && x.test(n)
      }.map { r =>
        mocked = true

        val news = try {
          implicit val ctx = new StaticECtx(n.attrs, Some(this.ctx), Some(a))
          val xx = r.apply(n, None)
          xx.collect{
            // collect resulting values in the context as well
            case v@EVal(p) => {
              ctx.put(p)
              v
            }
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
            List(DomAst(EError("Exception:" + e.getMessage), AstKinds.GENERATED))
        }

        newNodes = newNodes ::: news
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
            a.children append DomAst(TestResult("ok"), "test").withSpec(e)
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
            label("found", "warning") + " " +
              cole.highestMatching.map(_.diffs.values.map(_._1).toList.map(x => s"""<span style="color:red">$x</span>""").mkString(",")).mkString
          ),
          AstKinds.TEST
        ).withSpec(e)
      }
    }
  }

  /** test epected values */
  private def expandExpectV(a: DomAst, e: ExpectV) : Unit = {
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
      val vals = subtrees.flatMap{n=>
        n.collect {
          case d@DomAst(n: EVal, k, _, _) if AstKinds.isGenerated(k) => d
        }
      }

//      val vals = subtrees.flatMap{n=>
      subtrees.foreach {n=>
        val vvals = n.collect {
          case d@DomAst(n: EVal, k, _, _) if AstKinds.isGenerated(k) => d
        }

        // include the message's values in its context
        val newctx = new StaticECtx(n.value.asInstanceOf[EMsg].attrs, Some(ctx), Some(n))

        if (vvals.size > 0 && e.test(vvals.map(_.value.asInstanceOf[EVal].p), Some(cole), vvals)(newctx))
          a.children append DomAst(TestResult("ok"), AstKinds.TEST).withSpec(e)
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
            cole.highestMatching.map(_.diffs.values.map(_._1).toList.map(x => s"""<span style="color:red">$x</span>""").mkString(",")).mkString
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
        d.moreDetails = d.moreDetails + label("expected", "danger") + " " + s
      }

      // did some rules succeed?
      if (a.children.isEmpty) {
        // oops - add test failure
        a.children append DomAst(
          TestResult(
            "fail",
            label("found", "warning") + " " +
              cole.highestMatching.map(_.diffs.values.map(_._1).toList.map(x => s"""<span style="color:red">$x</span>""").mkString(",")).mkString
          ),
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

    import razie.diesel.ext.stripQuotes

    // collect values
    val values = root.collect {
      case d@DomAst(EVal(p), /*"generated"*/ _, _, _) if oattrs.isEmpty || oattrs.find(_.name == p.name).isDefined => p
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
      case d@DomAst(EVal(p), /*"generated"*/ _, _, _) if oattrs.isEmpty || oattrs.find(_.name == p.name).isDefined => p
    }

    values
  }

}

trait InfoNode // just a marker for useless info nodes

