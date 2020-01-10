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
import razie.diesel.dom._
import razie.diesel.engine._
import razie.diesel.engine.nodes.{EMsg, EVal, EnginePrep}
import razie.diesel.model.DieselMsg
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

    // 3. add the entry points / triggers to the process
    EnginePrep.addStoriesToAst(root, stories)

    // 4. rules configuration

    // 5. start processing
    val engine = DieselAppContext.mkEngine(dom, root, settings, stories ::: specs, "simpleFlow")

    engine
  }

  final val NOUSER = new ObjectId()

  /** execute message - this is the typical use of an engine
   * execute message to given reactor
    *
    * @param msg "entity.action(p=value,etc)
    * @param specs the specs to use for rules
    * @param stories any other stories to add (tests, engine settings etc)
    * @param settings engine settings
    * @return
    *
    * this is only used from the CQRS, internally - notice no request
    */
  def runDom(msg:String, specs:List[WID], stories: List[WID], settings:DomEngineSettings) : Future[Map[String,Any]] = {
    val realm = settings.realm getOrElse specs.headOption.map(_.getRealm).mkString
    val page = new WikiEntry("Spec", "fiddle", "fiddle", "md", "", NOUSER, Seq("dslObject"), realm)

    // don't audit diesel messages - too many
    if(
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

    val pages = (specs ::: stories).filter(_.section.isEmpty).flatMap(_.page)

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
    EnginePrep.addStoriesToAst(root, List(ipage))

    // start processing all elements
    val engine = DieselAppContext.mkEngine(
      dom plus idom,
      root,
      settings,
      ipage :: pages map WikiDomain.spec,
      DieselMsg.runDom+msg)

    engine.process.map { engine =>

      val errors = new ListBuffer[String]()

      // find the spec and check its result
      // then find the resulting value.. if not, then json
      val oattrs = dom.moreElements.collect {
        //      case n:EMsg if n.entity == e && n.met == a => n
        case n: EMsg if msg.startsWith(n.entity + "." + n.met) => n
      }.headOption.toList.flatMap(_.ret)

      if (oattrs.isEmpty) {
        errors append s"Can't find the spec for $msg"
      }

      // collect values
      val values = root.collect {
        case d@DomAst(EVal(p), /*AstKinds.GENERATED*/ _, _, _) if oattrs.isEmpty || oattrs.find(_.name == p.name).isDefined => (p.name, p.currentStringValue)
      }

      var m = Map(
//                   "value" -> values.headOption.map(_._2).map(stripQuotes).getOrElse(""),
        "value" -> engine.resultingValue,
        "values" -> values.toMap,
        "totalCount" -> (engine.totalTestCount),
        "failureCount" -> engine.failedTestCount,
        "errors" -> errors.toList,
        "root" -> root,
        "engineId" -> engine.id,
        "dieselTrace" -> DieselTrace(root, settings.node, engine.id, "diesel", "runDom", settings.parentNodeId).toJson
      )
      m
    }
  }

}

