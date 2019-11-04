/**
  *  ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.samples

import razie.cout
import razie.diesel.dom._
import razie.diesel.engine._
import razie.diesel.engine.nodes.EnginePrep
import razie.tconf.TextSpec
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by raz on 2017-06-13.
  */
object SimpleFlow {

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
    // 1. settings
    val settings = new DomEngineSettings()

    // 2. the current domain (classes, entities, message specs etc)
    val dom = RDomain.domFrom(specs.head, specs.tail)

    // 2. create the process instance / root node
    val root = DomAst("root", AstKinds.ROOT)

    // 3. add the entry points / triggers to the process
    EnginePrep.addStoriesToAst(root, List(story))

    // 4. rules configuration

    // 5. start processing
    val engine = DieselAppContext.mkEngine(dom, root, settings, story :: specs, "simpleFlow")

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


