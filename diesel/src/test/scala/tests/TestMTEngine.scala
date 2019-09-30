/**
  *   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package tests

import akka.actor.ActorSystem
import akka.testkit.TestKit
import razie.diesel.engine.{DieselAppContext, DomEngineSettings}
import razie.diesel.samples.DomEngineUtils

/**
  * Created by raz on 2017-07-06.
  */
class TestMTEngine extends TestKit(ActorSystem("x")) {

  import SampleSpecs1._

  DieselAppContext.setActorSystem(system)

//  override def afterAll {
//    TestKit.shutdownActorSystem(system)
//  }

  def run1 = {
  val engine = DomEngineUtils.execAndWait (DomEngineUtils.mkEngine (new DomEngineSettings (), specs, List (storySend) ) )

  println (engine.root.toString)
  println (engine.resultingValue)
  assert (engine.resultingValue contains "Jane")
}
}


