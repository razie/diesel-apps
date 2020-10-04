/*  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

import akka.actor.{Actor, Stash}
import java.util.concurrent.TimeUnit
import razie.audit.Audit
import razie.clog
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration


/** ====================== Actor infrastructure =============== */

/** base class for engine internal message */
trait DEMsg {
  def engineId: String
}

/** a message - request to decompose
  *
  * The engine will decompose the given node a and send to self a DERep
  */
case class DEReq (engineId:String, a:DomAst, recurse:Boolean, level:Int) extends DEMsg

/** a message - reply to decompose, basically asynchronous completion of a message
  *
  * The engine will stich the AST together and continue
  *
  * send this from async executors
  */
case class DERep      (engineId:String, a:DomAst, recurse:Boolean, level:Int, results:List[DomAst]) extends DEMsg
case class DEComplete (engineId:String, a:DomAst, recurse:Boolean, level:Int, results:List[DomAst]) extends DEMsg

/** initialize and stop the engine */
case object DEInit extends DEMsg { override def engineId = ""}
case object DEStop extends DEMsg { override def engineId = ""}

/** suspend the engine for async messages - to continue you have to fire off a DERep yourself, later */
case object DESuspend extends DEMsg { override def engineId = ""}

/** send the next message later */
case class DELater(engineId: String, d: Int, next: DEMsg, durationExpr: Option[String] = None) extends DEMsg

case class DEStartTimer (engineId:String, d:Int, results:List[DomAst]) extends DEMsg
case class DETimer      (engineId:String, results:List[DomAst]) extends DEMsg

/** error handling
  *
  * todo should not be part of normal processing DEMsg
  */
case class DEError      (engineId:String, msg:String) extends DEMsg

/**
  * engine router - routes updates to proper engine actor
  *
  * todo can i drop refMap and address actors directly?
  */
class DomEngineRouter () extends Actor {

  def receive = {
    case DEInit => {
      // todo is this fair?
      // in case other services don't start me - i will start myself
      DieselAppContext.getActorSystem.scheduler.scheduleOnce(
        Duration.create(7, TimeUnit.SECONDS),
        this.self,
        "startAll"
      )
    }

    // stop all engines
    case DEStop => {
      Audit.logdb("DEBUG", "DomEngineRouter STOP All")
      DieselAppContext.activeActors.values.map(_ ! DEStop)
   }

    case "startAll" => {
      Audit.logdb("DEBUG", "DomEngineRouter startAll")
      if (!DieselAppContext.serviceStarted) {
        Audit.logdb("DEBUG-BAD", "DomEngineRouter self init")
        DieselAppContext.start
      }
    }

    // all DE messages share routing
    case m : DEMsg => route(m.engineId, m)

  }

  def route (id:String, msg:DEMsg) = {
    DieselAppContext.activeActors.get(id).map(_ ! msg).getOrElse(
      clog << "DomEngine Router DROP message "+msg
      // todo recover failed workflows
      // todo distributed routing
    )
  }

}

/** each engine has its own actor
  *
  * it will serialize status udpates and execution
  *
  * an engine will parallelize as much as async is built-into their activities
  */
class DomEngineActor (eng:DomEngine) extends Actor with Stash {

  def checkInit : Boolean = {
    if(!DieselAppContext.serviceStarted) {
      // if not started, stash the message for now
      stash()
      false
    } else
      true
  }

  def receive = {

    case DEInit => {
      //save refs for active engines
      // started, take all stashed messages
      unstashAll()
    }

    case req @ DEReq(eid, a, r, l) if checkInit => {
      if(eng.id == eid) eng.processDEMsg(req)
      else DieselAppContext.router.map(_ ! req)
    }

    case rep @ DERep(eid, a, r, l, results) if checkInit => {
      checkInit
      if(eng.id == eid) {
        eng.processDEMsg(rep)
      } else {
        DieselAppContext.router.map(_ ! rep)
      }
    }

    case rep @ DEComplete(eid, a, r, l, results) if checkInit => {
      checkInit
      if(eng.id == eid) {
        eng.processDEMsg(rep)
      } else {
        DieselAppContext.router.map(_ ! rep)
      }
    }

    case DEStop => {
      //remove refs for active engines
      DieselAppContext.activeEngines.remove(eng.id)
      DieselAppContext.activeActors.remove(eng.id)
      context stop self
    }

    // used when engines schedule stuff
    case timer@DELater(id, d, m, durationExpr) => {
      // todo get this to work
//      val dur: FiniteDuration = durationExpr
//          .map(Duration.create)
//          .getOrElse(Duration.create(d, TimeUnit.MILLISECONDS))

      val dur = Duration.create(d, TimeUnit.MILLISECONDS)

      if (eng.id == id) {
        DieselAppContext.getActorSystem.scheduler.scheduleOnce(
          dur,
          this.self,
          m
        )
      }
      else DieselAppContext.router.map(_ ! timer)
    }

    case timer@DEStartTimer(id, d, m) => {
      // used when engines schedule stuff
      if (eng.id == id) {
        DieselAppContext.getActorSystem.scheduler.scheduleOnce(
          Duration.create(d, TimeUnit.MILLISECONDS),
          this.self,
          DETimer(id, m)
        )
      }
      else DieselAppContext.router.map(_ ! timer)
    }

    case timer @ DETimer(id,m) => {
      // used when engines schedule stuff
      if(eng.id == id) {
      }
      else DieselAppContext.router.map(_ ! timer)
    }
  }

  override def postStop() = {
    // assert it's stopped
    // DieselAppContext.activeActors.remove(eng.id)
  }
}


