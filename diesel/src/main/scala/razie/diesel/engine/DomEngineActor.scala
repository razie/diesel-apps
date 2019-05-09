/**
 *  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

import java.util.concurrent.TimeUnit

import akka.actor.{Actor, Stash}
import play.libs.Akka
import razie.audit.Audit
import razie.clog
import razie.diesel.dom.DomAst

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

/** a message - reply to decompose
  *
  * The engine will stich the AST together and continue
  */
case class DERep (engineId:String, a:DomAst, recurse:Boolean, level:Int, results:List[DomAst]) extends DEMsg
case class DEComplete (engineId:String, a:DomAst, recurse:Boolean, level:Int, results:List[DomAst]) extends DEMsg

/** initialize and stop the engine */
case object DEInit extends DEMsg { override def engineId = ""}
case object DEStop extends DEMsg { override def engineId = ""}

/** suspend the engine for async messages - to continue you have to fire off a DERep yourself, later */
case object DESuspend extends DEMsg { override def engineId = ""}

/** send the next message later */
case class  DELater (engineId:String, d:Int, next:DEMsg) extends DEMsg

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
      Akka.system.scheduler.scheduleOnce(
        Duration.create(7, TimeUnit.SECONDS),
        this.self,
        "startAll"
      )
    }

//    case req @ DEReq(id, a, r, l) => route(id, req)
//    case rep @ DERep(id, a, r, l, results) => route(id, rep)
//    case rep @ DEComplete(id, a, r, l, results) => route(id, rep)

    case DEStop => {
//      DieselAppContext.refMap.values.map(_ ! DEStop)
    }

    case "startAll" => {
      Audit.logdb("DEBUG", "DomEngineRouter startAll")
      if (!DieselAppContext.serviceStarted) {
        Audit.logdb("DEBUG-BAD", "DomEngineRouter self init")
        DieselAppContext.start
      }
    }

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

/** exec context for engine - each engine has its own.
  *
  * it will serialize status udpates and execution
  */
class DomEngineActor (eng:DomEngine) extends Actor with Stash {

  def checkInit : Boolean = {
    if(!DieselAppContext.serviceStarted) {
      stash()
      false
    } else
      true
  }

  def receive = {

    case DEInit => {
      //save refs for active engines
      unstashAll()
    }

    case req @ DEReq(eid, a, r, l) if checkInit => {
      if(eng.id == eid) eng.processDEMsg(req)
      else DieselAppContext.router.map(_ ! req)
    }

    case rep @ DERep(eid, a, r, l, results) if checkInit => {
      checkInit
      if(eng.id == eid) eng.processDEMsg(rep)
      else DieselAppContext.router.map(_ ! rep)
    }

    case rep @ DEComplete(eid, a, r, l, results) if checkInit => {
      checkInit
      if(eng.id == eid) {
        eng.processDEMsg(rep)
      }
      else DieselAppContext.router.map(_ ! rep)
    }

    case DEStop => {
      //remove refs for active engines
      DieselAppContext.activeEngines.remove(eng.id)
      DieselAppContext.activeActors.remove(eng.id)
      context stop self
    }

    case timer @ DELater(id,d,m) => {
      if(eng.id == id) {
        Akka.system.scheduler.scheduleOnce(
          Duration.create(d, TimeUnit.MILLISECONDS),
          this.self,
          m
        )
      }
      else DieselAppContext.router.map(_ ! timer)
    }

    case timer @ DEStartTimer(id,d,m) => {
      if(eng.id == id) {
        Akka.system.scheduler.scheduleOnce(
          Duration.create(d, TimeUnit.MILLISECONDS),
          this.self,
          DETimer(id,m)
        )
      }
      else DieselAppContext.router.map(_ ! timer)
    }

    case timer @ DETimer(id,m) => {
      if(eng.id == id) {
      }
      else DieselAppContext.router.map(_ ! timer)
    }
  }
}


