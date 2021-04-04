/**
  *  ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.samples

import org.bson.types.ObjectId
import razie.audit.Audit
import razie.ctrace
import razie.diesel.Diesel
import razie.diesel.dom.RDOM.P
import razie.diesel.dom._
import razie.diesel.engine._
import razie.diesel.engine.nodes.{EMsg, EVal, EnginePrep}
import razie.diesel.model.{DieselMsg, DieselMsgString, DieselTarget}
import razie.tconf.DSpec
import razie.wiki.model.{WID, WikiEntry}
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

/** utils and examples of using the engine */
object DomEngineUtils {

  // todo maybe not have this sync option at all?
  def execAndWait (engine:DomEngine, timeoutSec:Int = 50) = {
    val future = engine.process

    Await.result(future, Duration.create(timeoutSec, "seconds"))
  }

  // todo maybe not have this sync option at all?
  def execTestsAndWait (engine:DomEngine, timeoutSec:Int = 50) = {
    val future = engine.processTests

    Await.result(future, Duration.create(timeoutSec, "seconds"))
  }

  // todo reuse DomFiddle.runDom
  def mkEngine(settings: DomEngineSettings, specs : List[DSpec], stories:List[DSpec]) : DomEngine = {
    // 2. the current domain (classes, entities, message specs etc)
    val dom = RDomain.domFrom(specs.head, specs.tail)

    // 2. create the process instance / root node
    val root = DomAst("root", AstKinds.ROOT)

    val engine = DieselAppContext.mkEngine(dom, root, settings, stories ::: specs, "simpleFlow")

    // 3. add the entry points / triggers to the process
    EnginePrep.addStoriesToAst(engine, stories)

    engine
  }

  final val NOUSER = new ObjectId()

  /** execute message - this is the typical use of an engine
    * execute message to given reactor
    *
    * @param msg      "entity.action(p=value,etc)
    * @param specs    the specs to use for rules
    * @param stories  any other stories to add (tests, engine settings etc)
    * @param settings engine settings
    * @return
    *
    * this is only used from the CQRS, internally - notice no request
    */
  def runDom(msg: String, specs: List[WID], stories: List[WID], settings: DomEngineSettings,
             omsg: Option[DieselMsgString] = None): Future[Map[String, Any]] = {
    // start processing all elements
    val engine = createEngine(msg, specs, stories, settings, omsg)

    engine.process.map { engine =>
      extractResult(msg, omsg, engine)
    }
  }

  /** execute message - this is the typical use of an engine
    * execute message to given reactor
    *
    * @param msg      "entity.action(p=value,etc)
    * @param specs    the specs to use for rules
    * @param stories  any other stories to add (tests, engine settings etc)
    * @param settings engine settings
    * @return
    *
    * this is only used from the CQRS, internally - notice no request
    */
  def createEngine(msg: String, specs: List[WID], stories: List[WID], settings: DomEngineSettings,
                   omsg: Option[DieselMsgString] = None): DomEngine = {
    val realm = settings.realm getOrElse specs.headOption.map(_.getRealm).mkString
    val page = new WikiEntry("Spec", "fiddle", "fiddle", "md", "", NOUSER, Seq("dslObject"), realm)

    // don't audit diesel messages - too many
    if (
      msg.startsWith(DieselMsg.REALM.REALM_LOADED_MSG) ||
          msg.startsWith(DieselMsg.GUARDIAN_RUN) ||
          msg.startsWith(DieselMsg.GUARDIAN_POLL) ||
          msg.startsWith(DieselMsg.WIKI_UPDATED) ||
          msg.startsWith(DieselMsg.CRON_TICK) ||
          false
    ) {
      // clog?
    }
    else
      Audit.logdb("DIESEL_FIDDLE_RUNDOM ", msg)

    val pages = (specs ::: stories)
        .filter(_.section.isEmpty)
        .distinct
        .flatMap(_.page)

    // to domain
    val dom = WikiDomain.domFrom(page, pages)

    // make up a story
    val FILTER = Array("sketchMode", "mockMode", "blenderMode", "draftMode")
    var story = if (msg.trim.startsWith("$msg") || msg.trim.startsWith("$send")) msg else "$msg " + msg
    ctrace << "STORY: " + story

    // todo this has no EPos - I'm loosing the epos on sections
    // put together all sections
    val story2 = (specs ::: stories).filter(_.section.isDefined).flatMap(_.content).mkString("\n")
    story = story + "\n" + story2.lines.filterNot(x =>
      x.trim.startsWith("$msg") || x.trim.startsWith("$send")
    ).mkString("\n") + "\n"

    val ipage = new WikiEntry("Story", "fiddle", "fiddle", "md", story, NOUSER, Seq("dslObject"), realm)
    val idom = WikiDomain.domFrom(ipage).get.revise addRoot

    var res = ""

    val root = DomAst("root", "root")

    // start processing all elements
    val engine = DieselAppContext.mkEngine(
      dom plus idom,
      root,
      settings,
      ipage :: pages map WikiDomain.spec,
      DieselMsg.runDom + msg)

    EnginePrep.addStoriesToAst(engine, List(ipage))

    engine
  }

  /** extract result from engine
    */
  def extractResult(msg: String, omsg: Option[DieselMsgString], engine: DomEngine): Map[String, Any] = {
    val errors = new ListBuffer[String]()

    // find the spec and check its result
    // then find the resulting value.. if not, then json
    val oattrs = engine.dom.moreElements.collect {
      //      case n:EMsg if n.entity == e && n.met == a => n
      case n: EMsg if msg.startsWith(n.entity + "." + n.met) => n
    }.headOption.toList.flatMap(_.ret)

    if (oattrs.isEmpty) {
      errors append s"Can't find the spec for $msg"
    }

    // collect values
    val values = engine.root.collect {
      case d@DomAst(EVal(p), /*AstKinds.GENERATED*/ _, _, _) if oattrs.isEmpty || oattrs.find(
        _.name == p.name).isDefined => (p.name, p.currentStringValue)
    }

    val resValue = engine.extractFinalValue(omsg.flatMap(_.omsg).map(_.ea).mkString).map(_.currentStringValue).mkString

    var m = Map(
      "payload" -> resValue,
      "resValue" -> resValue,
      "value" -> engine.resultingValue,
      "values" -> values.toMap,
      "totalCount" -> (engine.totalTestCount),
      "failureCount" -> engine.failedTestCount,
      "errors" -> errors.toList,
      "root" -> engine.root,
      "engineId" -> engine.id,
      DieselTrace.dieselTrace -> DieselTrace(engine.root, engine.settings.node, engine.id, "diesel", "runDom",
        engine.settings.parentNodeId).toJson
    )
    m
  }

  /**
    * run message sync
    */
  def runMsgSync(msg: DieselMsg): Option[P] = {
    val fut = msg.toMsgString.startMsg

    // get timeout max from realm settings: paid gets more etc
    val res = Await.result(fut, Duration.create(30, "seconds"))
    res.get(Diesel.PAYLOAD)
        .map(p =>
          if (p.isInstanceOf[P]) p.asInstanceOf[P]
          else P.fromSmartTypedValue(Diesel.PAYLOAD, p.toString)
//          else P.fromSmartTypedValue(Diesel.PAYLOAD, new RuntimeException(p.toString))
        )
  }

  /**
    * run message sync
    */
  def runMsgSync(realm: String, m: EMsg): Option[P] = {
    runMsgSync(new DieselMsg(m, DieselTarget.ENV(realm)))
  }

  /**
    * run message sync
    */
  def runMsgSync(realm: String, e: String, a: String, parms: Map[String, Any]): Option[P] = {
    runMsgSync(DieselMsg(e, a, parms, DieselTarget.ENV(realm)))
  }
}

