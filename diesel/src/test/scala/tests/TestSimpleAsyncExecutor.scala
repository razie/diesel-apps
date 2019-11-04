/**
  *   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package tests

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import org.scalatest.{MustMatchers, OptionValues, WordSpecLike}
import razie.diesel.engine.exec.EExecutor
import razie.diesel.engine.nodes.{EInfo, EMsg}
import razie.diesel.engine.{DEMsg, DieselAppContext, DomAst, DomEngineSettings}
import razie.diesel.expr.ECtx
import razie.diesel.ext.MatchCollector
import razie.diesel.samples.DomEngineUtils
import razie.wiki.parser.DieselTextSpec

/* a sync executor */
class EESyncExec extends EExecutor("testSync") {
  /** what messages do I apply to */
  override def test(m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) = {
    m.entity == "sync"
  }

  override def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] = {
    EInfo ("Sync exec done") :: Nil
  }
}

/* an async executor */
class EEAsyncExec extends EExecutor("testAsync") {
  /** what messages do I apply to */
  override def test(m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) = {
    m.entity == "async"
  }

  override def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] = {
    razie.Threads.fork {
      Thread.sleep(2000)

      // here you cannot assyme you have control of the engine in any way... all you can do is send it messages
      // you need to remember the branch where you forked off
      val parent = null

      // send async to engine
//      case class DERep      (engineId:String, a:DomAst, recurse:Boolean, level:Int, results:List[DomAst]) extends DEMsg
    }
    EInfo ("Async exec started...") :: Nil
  }
}

/**
  * example of async executor
  */
class TestSimpleAsyncExecutor extends WordSpecLike with MustMatchers with OptionValues {

  // create an actor system or use the default etc
  implicit val system = ActorSystem("testsystem", ConfigFactory.parseString(""" """))
  DieselAppContext
      .withSimpleMode()
      .withActorSystem(system)


  "simplest engine" should {
    "execute a message" in {
      // make a spec
      val spec = DieselTextSpec (
        "spec_name",
        """
          |$when home.guest_arrived => lights.on
          |
          |$when home.guest_arrived(name=="Jane") => chimes.welcome(name)
          |
          |$when chimes.welcome => (greeting = "Greetings, "+name)
          |""".stripMargin
      )

      // make a story
      val story = DieselTextSpec (
        "story_name",
        """
          |$send home.guest_arrived(name="Jane")
          |""".stripMargin
      )

      // run it: create engine, run story and wait for result
      val engine = DomEngineUtils.execAndWait(
        DomEngineUtils.mkEngine(
          new DomEngineSettings().copy(realm=Some("rk")),
          List(spec),
          List(story)
        )
      )

      println(engine.root.toString)    // debug trace of engine's execution
      println(engine.resultingValue)   // resulting value, if any

      // test it
      assert(engine.resultingValue contains "Greetings, Jane")
    }
  }

//  import SampleSpecs1._
//
//  "simple specs" should {
//    "execute a message" in {
//      val engine = DomEngineUtils.execAndWait(DomEngineUtils.mkEngine(new DomEngineSettings().copy(realm=Some("rk")), specs, List(storySend)))
//      println(engine.root.toString)
//      println(engine.resultingValue)
//      assert(engine.resultingValue contains "Jane")
//    }
//  }
}

