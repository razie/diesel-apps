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
import razie.diesel.engine.{DieselAppContext, DomEngineSettings}
import razie.diesel.samples.DomEngineUtils
import razie.wiki.parser.DieselTextSpec

/**
  * Created by raz on 2017-07-06.
  */
class TestSimpleEngine extends WordSpecLike with MustMatchers with OptionValues {

  // create an actor system or use the default etc

  implicit val system = ActorSystem("testsystem", ConfigFactory.parseString(""" """))

  // tell the engine to use this system
  DieselAppContext.setActorSystem(system)

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

