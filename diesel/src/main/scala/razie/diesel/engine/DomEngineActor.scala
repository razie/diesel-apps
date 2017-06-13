/**
 *  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

import java.util.concurrent.TimeUnit

import akka.actor.Actor
import play.libs.Akka
import razie.clog
import razie.diesel.dom.DomAst

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration


/** ====================== Actor infrastructure =============== */

/** base class for engine internal message */
class DEMsg ()

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

/** initialize and stop the engine */
case object DEInit extends DEMsg
case object DEStop extends DEMsg

case class DEStartTimer (engineId:String, d:Int, results:List[DomAst]) extends DEMsg
case class DETimer      (engineId:String, results:List[DomAst]) extends DEMsg

case class DEError      (engineId:String, msg:String) extends DEMsg

/**
  * engine router - routes updates to proper engine actor
  */
class DomEngineRouter () extends Actor {

  def receive = {
    case DEInit => { }

    case req @ DEReq(id, a, r, l) => {
      DieselAppContext.refMap.get(id).map(_ ! req).getOrElse(
        clog << "DomEngine Router DROP message "+req
      )
    }

    case rep @ DERep(id, a, r, l, results) => {
      DieselAppContext.refMap.get(id).map(_ ! rep).getOrElse(
        clog << "DomEngine Router DROP message "+rep
      )
    }

    case DEStop => {
//      DieselAppContext.refMap.values.map(_ ! DEStop)
    }
  }
}

/** exec context for engine - each engine has its own.
  *
  * it will serialize status udpates and execution
  */
class DomEngineActor (eng:DomEngine) extends Actor {

  def receive = {
    case DEInit => {
      //save refs for active engines
    }

    case req @ DEReq(eid, a, r, l) => {
      if(eng.id == eid) eng.processDEMsg(req)
      else DieselAppContext.router.map(_ ! req)
    }

    case rep @ DERep(eid, a, r, l, results) => {
      if(eng.id == eid) eng.processDEMsg(rep)
      else DieselAppContext.router.map(_ ! rep)
    }

    case DEStop => {
      //remove refs for active engines
//      DieselAppContext.engMap.remove(eng.id)
      DieselAppContext.refMap.remove(eng.id)
      context stop self
    }

    case timer @ DEStartTimer(id,d,m) => {
      if(eng.id == id) {
        Akka.system.scheduler.scheduleOnce(
          Duration.create(1, TimeUnit.MINUTES),
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


