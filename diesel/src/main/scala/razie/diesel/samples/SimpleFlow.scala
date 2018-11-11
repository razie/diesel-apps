package razie.diesel.samples

import razie.{cout}
import razie.diesel.dom._
import razie.diesel.engine._
import razie.diesel.ext.{EMsg, EVal}
import razie.tconf.{DSpec, TextSpec}

import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

/**
  * Created by raz on 2017-06-13.
  */
object SimpleFlow {

  // some rules - make sure each line starts with $ and ends with \n
  val specs = List(
    TextSpec ( "spec1",
      """
$when home.guest_arrived(name) => lights.on

$when home.guest_arrived(name=="Jane") => chimes.welcome(name="Jane")
      """.stripMargin
    ),

    TextSpec ( "spec2",
      """
$mock chimes.welcome => (greeting = "Greetings, "+name)
      """.stripMargin
    )
  )

  // some trigger message
  val story =
    TextSpec ( "story1",
      """
$msg home.guest_arrived(name="Jane")
      """.stripMargin
    )

  def main (argv:Array[String]) : Unit = {
    // 1. settings
    val settings = new DomEngineSettings()

    // 2. the current domain (classes, entities, message specs etc)
    val dom = RDomain.domFrom(specs.head, specs.tail)

    // 2. create the process instance / root node
    val root = DomAst("root", AstKinds.ROOT)

    // 3. add the entry points / triggers to the process
    RDExt.addStoryToAst(root, List(story))

    // 4. rules configuration

    // 5. start processing
    val engine = DieselAppContext.mkEngine(dom, root, settings, story :: specs, "simpleFlow")

    // 6. when done...
    val future = engine.process

    future.map { engine =>

      val root = engine.root // may be a different

      cout << "DONE ---------------------- "
      cout << root.toString
    }

    // just hang around to let the engine finish
    Thread.sleep(5000)
  }

}

/**
  * Created by raz on 2017-06-13.
  */
object SimplestFlow {

  // some rules - make sure each line starts with $ and ends with \n
  val specs = List(
    TextSpec ( "spec1",
      """
$when home.guest_arrived(name) => lights.on

$when home.guest_arrived(name=="Jane") => chimes.welcome(name="Jane")
      """.stripMargin
    ),

    TextSpec ( "spec2",
      """
$mock chimes.welcome => (greeting = "Greetings, "+name)
      """.stripMargin
    )
  )

  // some trigger message
  val story =
    TextSpec ( "story1",
      """
$msg home.guest_arrived(name="Jane")
      """.stripMargin
    )

  def main (argv:Array[String]) : Unit = {
    val engine = DomEngineUtils.mkEngine(new DomEngineSettings(), specs, List(story))

    // 6. when done...
    val future = engine.process

    future.map { engine =>

      val root = engine.root // may be a different

      cout << "DONE ---------------------- "
      cout << root.toString
    }

    // just hang around to let the engine finish
    Thread.sleep(5000)
  }
}

object DomEngineUtils {

  // todo maybe not have this sync option at all?
  def execAndWait (engine:DomEngine) = {
    val future = engine.process

    Await.result(future, Duration.create(5, "seconds"))
  }

  // todo maybe not have this sync option at all?
  def execTestsAndWait (engine:DomEngine) = {
    val future = engine.processTests

    Await.result(future, Duration.create(5, "seconds"))
  }

  // todo reuse DomFiddle.runDom
  def mkEngine(settings: DomEngineSettings, specs : List[DSpec], stories:List[DSpec]) : DomEngine = {
    // 2. the current domain (classes, entities, message specs etc)
    val dom = RDomain.domFrom(specs.head, specs.tail)

    // 2. create the process instance / root node
    val root = DomAst("root", AstKinds.ROOT)

    // 3. add the entry points / triggers to the process
    RDExt.addStoryToAst(root, stories)

    // 4. rules configuration

    // 5. start processing
    val engine = DieselAppContext.mkEngine(dom, root, settings, stories ::: specs, "simpleFlow")

    engine
  }

  /** execute message
    *
    * @param msg "entity.action(p=value,etc)
    * @param specs the specs to use for rules
    * @param stories any other stories to add (tests, engine settings etc)
    * @param settings engine settings
    * @return
    */
  def runDom(msg:String, specs:List[DSpec], stories: List[DSpec], settings:DomEngineSettings) : Future[Map[String,Any]] = {
//    Audit.logdb("DIESEL_RUNDOM")

    // to domain
    val dom = RDomain.domFrom(specs.head, specs.tail)

    // make up a story
    var originalMsg = msg
    var story = if (msg.trim.startsWith("$msg")) {
      originalMsg = msg.trim.replace("$msg ", "").trim
      msg
    } else "$msg " + msg

    val idom =
      if(stories.isEmpty) RDomain.empty
      else RDomain.domFrom(stories.head, stories.tail).revise addRoot

    var res = ""

    val root = DomAst("root", AstKinds.ROOT)
    RDExt.addStoryToAst(root, stories)

    // start processing all elements
    val engine = DieselAppContext.mkEngine(dom plus idom, root, settings, stories ::: specs, "simpleFlow")

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

      import razie.diesel.ext.stripQuotes

      // collect values
      val values = root.collect {
        case d@DomAst(EVal(p), /*AstKinds.GENERATED*/ _, _, _) if oattrs.isEmpty || oattrs.find(_.name == p.name).isDefined => (p.name, p.dflt)
      }

      var m = Map(
        "value" -> values.headOption.map(_._2).map(stripQuotes).getOrElse(""),
        "values" -> values.toMap,
        "failureCount" -> engine.failedTestCount,
        "errors" -> errors.toList,
        "root" -> root,
        "dieselTrace" -> DieselTrace(root, settings.node, engine.id, "diesel", "runDom", settings.parentNodeId).toJson
      )
      m
    }
  }

}

