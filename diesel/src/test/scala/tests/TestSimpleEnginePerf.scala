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
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}

/**
  * Created by raz on 2017-07-06.
  */
class TestSimpleEnginePerf extends WordSpecLike with MustMatchers with OptionValues {

  val THREADS = 20
  val CYCLES  = 1000
  val SLEEP = false

  implicit val system = ActorSystem("testsystem", ConfigFactory.parseString(""" """))
  DieselAppContext
      .withSimpleMode()
      .withActorSystem(system)

  // COMMENT THE REMOTE stories to get just a perf test measure
  val rspecs  =
    DieselUrlTextSpec("http://specs.dieselapps.com/wikie/content/Spec:expr_spec", "expr_spec") ::
    DieselUrlTextSpec("http://specs.dieselapps.com/wikie/content/Spec:expr-json-spec", "expr-json-spec") ::
//    DieselUrlTextSpec("http://specs.dieselapps.com/wikie/content/Spec:rest_spec", "rest-spec") ::
    Nil

  // COMMENT THE REMOTE stories to get just a perf test measure
  val rstories =
    DieselUrlTextSpec("http://specs.dieselapps.com/wikie/content/Story:expr_story", "expr_story") ::
    DieselUrlTextSpec("http://specs.dieselapps.com/wikie/content/Story:expr-json-story", "expr-json-story") ::
//      DieselUrlTextSpec("http://specs.dieselapps.com/wikie/content/Story:rest_story", "rest_story") ::
      Nil

  val flowCount = new AtomicInteger(0)
  val testCount = new AtomicInteger(0)
  val failCount = new AtomicInteger(0)

  // the sleep part
  val sleepspec =
      DieselTextSpec(
        "sleep_spec",
        s"""
           |
           |$$when perftest.guest_arrived => ctx.sleep(duration=10)
           |
           |""".stripMargin
      )

  // make a story
  val story =
      DieselTextSpec(
        "story_name",
        s"""
           |$$send perftest.guest_arrived(name="Jane")
           |""".stripMargin
      )

  razie.Log.log(s"Starting... CYCLES=$CYCLES THREADS=$THREADS")

  "simplest engine" should {

    "warmup" in {

      DomEngineUtils.execAndWait(
        DomEngineUtils.mkEngine(
          new DomEngineSettings().copy(realm = Some("rk")),
          List(sleepspec) ::: rspecs, // IMPORTANT: rspecs need warmup, see below
          List(story) ::: rstories    // IMPORTANT: rstories need warmup, see below
        )
      )

      // todo WE mt-safe
      // rspecs and rstories need warmup = they are not multithread-safe. If a WE instance is parsed in parallel,
      // it will have issues
    }

    // make a spec
    def spec(i:Long, j:Long) =
      DieselTextSpec(
        "spec_name",
        s"""
           |
           |$$when perftest.guest_arrived(name=="Jane") => perftest.welcome(name)
           |
           |$$when perftest.welcome => (greeting = "Greetings, "+name+$i+"-"+$j)
           |
           |""".stripMargin
      )

    var start = System.currentTimeMillis()

    clog << s"-------------- starting $CYCLES cycles with $THREADS threads"

    "run well in many threads " in {

      start = System.currentTimeMillis()
      razie.Threads.forkjoin(Range(0, THREADS)) { i =>
        Range(0, CYCLES).foreach { j =>

          // run it: create engine, run story and wait for result
          val engine = DomEngineUtils.execAndWait(
            DomEngineUtils.mkEngine(
              new DomEngineSettings().copy(realm = Some("rk")),
              (if(SLEEP) List(sleepspec) else Nil) ::: spec(i,j) :: rspecs,
              rstories ::: story :: Nil // i,j story at end, so it's the resultingValue
            ),
            1000 // a lot as some engines may take a while to complete
          )

//          println(engine.root.toString)    // debug trace of engine's execution
          println("------------- " + engine.resultingValue)   // resulting value, if any

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
      clog << s"DURATION: $end - $start = $dur"

      clog << "==========================================="
      clog << DieselAppContext.activeEngines.values.toList.mkString("\n")
      DieselAppContext.activeEngines.values.toList.map { e=>
        clog << s"xxxxxxxxxxxxxx engine ${e.id}"
        clog << e.root.toString
      }

      assert(DieselAppContext.activeEngines.size == 0)
      assert(DieselAppContext.activeActors.size == 0)

      val perf = if(dur > 0) flowCount.get() / (dur * 1.0) else -1
      clog << s"T O T A L  $testCount tests"
      clog << s"T O T A L  $flowCount flows in $dur seconds meaning $perf per sec"
    }
  }
}

