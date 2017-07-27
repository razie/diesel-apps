package tests

import org.scalatest._
import org.scalatest.Assertions._
import razie.diesel.dom.{DomState, TextSpec}
import razie.diesel.engine.DomEngineSettings
import razie.diesel.samples.DomEngineUtils

/**
  * Created by raz on 2017-07-06.
  */
class SimpleSpec extends FlatSpec {

    "simple specs" should "work ok" in {
      assert(1 == 1)
    }

  it should "concatenate specs" in {
    val engine = DomEngineUtils.execAndWait(DomEngineUtils.mkEngine(new DomEngineSettings(), specs, List(story)))
    assert(engine.resultingValue contains "Jane")
  }

  it should "concatenate specs" in {
    val engine = DomEngineUtils.execAndWait(DomEngineUtils.mkEngine(new DomEngineSettings(), specs, List(story)))
    assert(engine.resultingValue contains "Jane")
  }

  val specs = List(
    TextSpec ( "spec1",
"""
$when home.guest_arrived(name) => lights.on
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
  }

