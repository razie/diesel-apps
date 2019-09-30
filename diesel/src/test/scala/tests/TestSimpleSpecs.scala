/**
  *   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package tests

import org.scalatest.{MustMatchers, OptionValues, WordSpecLike}

/**
  * Use the SampleTextSpec wiht a domain parser, to test domain parsing
  */
class TestSimpleSpecs extends WordSpecLike with MustMatchers with OptionValues {

  import SampleSpecs1._

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
}

