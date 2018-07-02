package tests

import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.scalatest.{MustMatchers, OptionValues, WordSpecLike}
import razie.diesel.engine.{DieselAppContext, DomEngineSettings}
import razie.diesel.samples.DomEngineUtils

/**
  * Created by raz on 2017-07-06.
  */
class TestSimpleSpec extends TestKit(ActorSystem("x")) with WordSpecLike with MustMatchers with OptionValues {

  import SampleSpecs1._

  DieselAppContext.setActorSystem(system)

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "can parse" should {
    "nothing" in {
      assert(1 == 1)
    }


    "$send" in {
      val x = List(storySend).map(_.xparsed)
      println(x.map(_.collector.mkString))
      assert(x.map(_.collector.mkString).mkString contains "home.guest_arrived")
    }

    "$expect" in {
      val x = List(storyExpect).map(_.xparsed)
      assert(x.map(_.collector.mkString).mkString contains "greeting")
    }

    "$when" in {
      val x = specs.map(_.xparsed)
      assert(x.map(_.collector.mkString).mkString contains "home.guest_arrived")
    }

    "$when multiline" in {
      val x = specs.map(_.xparsed)
      assert(x.map(_.collector.mkString).mkString contains "home.guest_arrived")
    }

    "$mock" in {
      val x = specs.map(_.xparsed)
      assert(x.map(_.collector.mkString).mkString contains "some.mock")
    }

    "$val" in {
      val x = specs.map(_.xparsed)
      assert(x.map(_.collector.mkString).mkString contains "someval")
    }
  }

  "simple specs" should {
    "execute a message" in {
      val engine = DomEngineUtils.execAndWait(DomEngineUtils.mkEngine(new DomEngineSettings(), specs, List(storySend)))
      println(engine.root.toString)
      println(engine.resultingValue)
      assert(engine.resultingValue contains "Jane")
    }
  }
}


