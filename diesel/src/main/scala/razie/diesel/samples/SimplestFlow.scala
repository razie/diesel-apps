/**
  *  ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.samples

import razie.cout
import razie.diesel.engine._
import razie.tconf.TextSpec
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by raz on 2017-06-13.
  */
object SimplestFlow {

  // some rules - make sure each line starts with $ and ends with \n
  val specs = List(
    TextSpec ( "spec1",
      """
$when home.guest_arrived(name) => lights.on

$when home.guest_arrived(name=="Jane") => chimes.welcome(name="Jane")
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

  def main (argv:Array[String]) : Unit = {
    val engine = DomEngineUtils.mkEngine(new DomEngineSettings(), specs, List(story))

    // 6. when done...
    val future = engine.process

    future.map { engine =>

      val root = engine.root // may be a different

      cout << "DONE ---------------------- "
      cout << root.toString
    }

    // just hang around to let the engine finish
    Thread.sleep(5000)
  }
}


