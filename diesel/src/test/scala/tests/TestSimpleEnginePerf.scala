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

  val THREADS = 20
  val CYCLES  = 50000
  val SLEEP = false

  implicit val system = ActorSystem("testsystem", ConfigFactory.parseString(""" """))
  DieselAppContext
      .withSimpleMode()
      .withActorSystem(system)
      .initExecutors

  razie.Log.log("Starting...")

  val rspec = DieselTextSpec("r", "") //DieselUrlTextSpec("http://specs.dieselapps.com/wikie/content/Spec:expr_spec", "expr_spec")
  val rstory = DieselTextSpec("r", "") //DieselUrlTextSpec("http://specs.dieselapps.com/wikie/content/Story:expr_story", "expr_story")

  val flowCount = new AtomicInteger(0)
  val testCount = new AtomicInteger(0)
  val failCount = new AtomicInteger(0)

  // the sleep part
  val sleepspec =
      DieselTextSpec(
        "sleep_spec",
        s"""
           |
           |$$when home.guest_arrived => ctx.sleep(duration=10)
           |
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

  "simplest engine" should {

    "warmup" in {

      DomEngineUtils.execAndWait(
        DomEngineUtils.mkEngine(
          new DomEngineSettings().copy(realm = Some("rk")),
          List(story),
          List(story)
        )
      )
    }

    var start = System.currentTimeMillis()

    "run well in many threads " in {

      start = System.currentTimeMillis()
      razie.Threads.forkjoin(Range(0, THREADS)) { i =>
        Range(0, CYCLES).foreach { j =>

          // make a spec
          val spec =
            rspec +
//            sleepspec +
            DieselTextSpec(
                "spec_name",
                s"""
                   |
                   |$$when home.guest_arrived(name=="Jane") => chimes.welcome(name)
                   |
                   |$$when chimes.welcome => (greeting = "Greetings, "+name+$i+"-"+$j)
                   |
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

//          println(engine.root.toString)    // debug trace of engine's execution
//          println(engine.resultingValue)   // resulting value, if any

          flowCount.incrementAndGet()
          testCount.addAndGet(engine.totalTestCount)
          failCount.addAndGet(engine.failedTestCount)

          // test it
          assert(engine.resultingValue contains s"Greetings, Jane$i-$j")
        }
      }
    }

    "no failures" in {
      clog << "==========================================="
      clog << "Failures: " + failCount.get()

      assert(failCount.get() == 0)
    }

    "cleanup well" in {
      val end = System.currentTimeMillis()
      val dur = (end-start)/1000

      clog << "==========================================="
      clog << DieselAppContext.activeEngines.values.toList.mkString("\n")

      assert(DieselAppContext.activeEngines.size == 0)
      assert(DieselAppContext.activeActors.size == 0)

      val perf = if(dur > 0) flowCount.get() / dur else -1
      clog << s"T O T A L  $testCount tests"
      clog << s"T O T A L  $flowCount flows in $dur seconds meaning $perf per sec"
    }
  }
}

