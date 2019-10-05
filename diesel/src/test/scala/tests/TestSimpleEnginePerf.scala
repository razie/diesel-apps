/**
  *   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package tests

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import java.util.concurrent.atomic.AtomicInteger
import org.scalatest.{MustMatchers, OptionValues, WordSpecLike}
import razie.clog
import razie.diesel.engine.{DieselAppContext, DomEngineSettings, RDExt}
import razie.diesel.samples.DomEngineUtils
import razie.wiki.parser.{DieselTextSpec, DieselUrlTextSpec}

/**
  * Created by raz on 2017-07-06.
  */
class TestSimpleEnginePerf extends WordSpecLike with MustMatchers with OptionValues {

  val THREADS = 100
  val CYCLES  = 10000

  implicit val system = ActorSystem("testsystem", ConfigFactory.parseString(""" """))
  DieselAppContext.setActorSystem(system)
  DieselAppContext.withSimpleMode()
  DieselAppContext.initExecutors

  razie.Log.log("Starting...")

  val testCount = new AtomicInteger(0)
  val failCount = new AtomicInteger(0)

  "simplest engine" should {

    "run well in many threads " in {

      val rspec = DieselUrlTextSpec("http://specs.dieselapps.com/wikie/content/Spec:expr_spec", "expr_spec")
      val rstory = DieselUrlTextSpec("http://specs.dieselapps.com/wikie/content/Story:expr_story", "expr_story")

      razie.Threads.forkjoin(Range(0, THREADS)) { i =>
        Range(0, CYCLES).foreach { j =>
          // make a spec
          val spec = rspec +
              DieselTextSpec(
                "spec_name",
                s"""
                   |$$when home.guest_arrived => ctx.sleep(duration=10)
                   |
                   |$$when home.guest_arrived(name=="Jane") => chimes.welcome(name)
                   |
                   |$$when chimes.welcome => (greeting = "Greetings, "+name+$i+"-"+$j)
                   |""".stripMargin
              )

          // make a story
          val story = rstory +
              DieselTextSpec(
                "story_name",
                s"""
                   |$$send home.guest_arrived(name="Jane")
                   |""".stripMargin
              )

          // run it: create engine, run story and wait for result
          val engine = DomEngineUtils.execAndWait(
            DomEngineUtils.mkEngine(
              new DomEngineSettings().copy(realm = Some("rk")),
              List(spec),
              List(story)
            )
          )

          println(engine.root.toString)    // debug trace of engine's execution
          println(engine.resultingValue)   // resulting value, if any

          testCount.addAndGet(engine.totalTestCount)
          failCount.addAndGet(engine.failedTestCount)

          // test it
          assert(engine.resultingValue contains s"Greetings, Jane$i-$j")
        }
      }
    }

    "no failures" in {
      clog << "==========================================="
      clog << "FAilures: " + failCount.get()

      assert(failCount.get() == 0)
    }

    "cleanup well" in {
      clog << "==========================================="
      clog << DieselAppContext.activeEngines.values.toList.mkString("\n")

      assert(DieselAppContext.activeEngines.size == 0)
      assert(DieselAppContext.activeActors.size == 0)

      clog << s"T O T A L  $testCount tests"
    }
  }
}

