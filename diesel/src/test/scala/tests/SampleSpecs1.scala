package tests

import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.scalatest.{MustMatchers, OptionValues, WordSpecLike}
import razie.diesel.engine.{DieselAppContext, DomEngineSettings}
import razie.diesel.samples.DomEngineUtils

/**
  * Created by raz on 2017-07-06.
  */
object SampleSpecs1 {
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


