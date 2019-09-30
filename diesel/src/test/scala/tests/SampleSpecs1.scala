/**
  *   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package tests

import razie.wiki.parser.DieselTextSpec

/**
  * sample stories and specs
  */
object SampleSpecs1 {

  // 1. the specs: setup rules configuration
  val specs = List(
    DieselTextSpec("spec1",
      """
$when home.guest_arrived(name) => lights.on
$when home.guest_arrived(name == "Jane") => chimes.welcome(name)
""".stripMargin
    ),

    DieselTextSpec("spec2",
      """
$when chimes.welcome(name) => (greeting = "Greetings, "+name)
""".stripMargin
    ),

    DieselTextSpec("spec3",
      """
$val aval="someval"

$mock some.mock(name) => (greeting = "Greetings, "+name)
""".stripMargin
    )
  )

  // a multi-line spec
  val specMultline =
    DieselTextSpec("multiline",
      """
$when home.guest_arrived(name == "Jane")
=> chimes.welcome(name)
=> (ivan="terrible")
=> do.something.multiline(name)
""".stripMargin
    )

  // 2. stories: some trigger message/test
  val storySend =
    DieselTextSpec("story1",
      """
$send home.guest_arrived(name="Jane")
""".stripMargin
    )

  val storyExpect =
    DieselTextSpec("story1",
      """
$expect (greeting contains "Jane")
""".stripMargin
    )
}

