package tests

import razie.diesel.dom._
import razie.diesel.engine.DomEngineSettings
import razie.diesel.samples.DomEngineUtils
import org.scalatestplus.play._
import play.api.{Application, Play}
import play.api.inject.guice._
import razie.tconf.parser.JMapFoldingContext
import razie.tconf.{DSpec, DTemplate, SpecPath, TSpecPath, TextSpec, BaseTextSpec}
import razie.wiki.parser.{DomParser, SimpleSpecParser}

import scala.collection.mutable

/**
  * Created by raz on 2017-07-06.
  */
//class SimpleSpec extends FlatSpec {
class TestSimpleEngine extends PlaySpec /*with GuiceOneAppPerSuite */ {

  val application: Application = new GuiceApplicationBuilder()
    .configure("some.configuration" -> "value")
    .build()
  Play.start(application)

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

  // 1. setup rules configuration
  val specs = List(
    SampleTextSpec("spec1",
      """
$when home.guest_arrived(name) => lights.on
$when home.guest_arrived(name == "Jane") => chimes.welcome(name)
""".stripMargin
    ),

    SampleTextSpec("spec2",
      """
$when chimes.welcome(name) => (greeting = "Greetings, "+name)
""".stripMargin
    ),

    SampleTextSpec("spec3",
      """
$val aval="someval"

$mock some.mock(name) => (greeting = "Greetings, "+name)
""".stripMargin
    )
  )

  val specMultline =
    SampleTextSpec("multiline",
      """
$when home.guest_arrived(name == "Jane")
=> chimes.welcome(name)
=> (ivan="terrible")
=> do.something.multiline(name)
""".stripMargin
    )

  // 2. some trigger message/test
  val storySend =
    SampleTextSpec("story1",
      """
$send home.guest_arrived(name="Jane")
""".stripMargin
    )

  val storyExpect =
    SampleTextSpec("story1",
      """
$expect (greeting contains "Jane")
""".stripMargin
    )
}

/** the simplest spec - from a named string property */
case class SampleTextSpec (override val name:String, override val text:String) extends BaseTextSpec(name, text) {
  override def mkParser = new SampleTextParser("rk")
}

class SampleTextParser(val realm: String) extends SimpleSpecParser with DomParser {
  withBlocks(domainBlocks)
}

