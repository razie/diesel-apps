package controllers

import akka.actor.{Actor, ActorRef, Props}
import play.api.mvc.{Result}
import razie.diesel.engine.DieselAppContext.getActorSystem
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import akka.util.Timeout
import akka.pattern.ask
import razie.audit.Audit

/** simple code pills controller
  *
  * pills are called with /pill/name
  */
//@Singleton
object CodePills extends RazController {

  sealed abstract class BasePill(val name: String) {
    def run (request:RazRequest) : Result
  }

  /** simple pill wrapper */
  private class Pill1(override val name: String, body: RazRequest => Result) extends BasePill(name) {
    def run (stok:RazRequest) : Result = body(stok)
  }

  /** text pill wrapper */
  private class Pill2(override val name: String, body: RazRequest => String) extends BasePill(name) {
    def run (stok:RazRequest) : Result = Ok(body(stok)).as("application/text")
  }

  val map = new AsyncMap[BasePill]
  implicit val timeout = Timeout(5 seconds)

  type T = BasePill

  /** add a pill by name with the given body */
  def add(name: String)(body: RazRequest => Result): Unit = {
    map.put (name, new Pill1(name, body))
  }

  /** add a text pill by name with the given body */
  def addString(name: String)(body: RazRequest => String): Unit = {
    map.put (name, new Pill2(name, body))
  }

  // routes pill/:name?attrs
  def run(pill: String) = RAction.async { implicit request =>
    map.get(pill).map {op=>
      op.map {
        p=> try {
          p.run(request)
        } catch {
          case e : Throwable => {
            Audit.logdb("ERR_CODE_PILLS", "pill: " + pill, e)
            throw new IllegalStateException(e)
          }
        }
      } getOrElse {
        Unauthorized("pill not found")
      }
    }
  }

  // initialize the thing
  def init = {
    add ("list") {request=>
      Ok("codePills: \n" + Await.result(map.list, timeout.duration));
    }
  }

  init
}

/** async map
  */
class AsyncMap[T] {

  private val p = Props(new AsyncMapActor[T]())
  private var map : Option[ActorRef] = Some(getActorSystem.actorOf(p, "codePillsMap"))

  implicit val timeout = Timeout(5 seconds)

  def put (k:String, v:T) = {
    map.get ! (k -> v)
  }

  def get (k:String): Future[Option[T]] = {
    (map.get ? k).asInstanceOf[Future[Option[T]]]
  }

  def list : Future[String] = {
    (map.get ? "codePillsActor::list").asInstanceOf[Future[String]]
  }
}


/** a generic map actor, just have an async map
  *
  * put by sending a tuple and get by sending a key
  *
  * when getting use the !? and get the response as an Option
  */
class AsyncMapActor[T] extends Actor {
  private val myMap = new mutable.HashMap[String, T]()

  private def put (k:String, v:T) = {
    self ! (k -> v)
  }

  private def get (k:String): Future[Option[T]] = {
    (self ? k)(Timeout(5 seconds)).asInstanceOf[Future[Option[T]]]
  }

  private def list : Future[String] = {
    (self ? "codePillsActor::list")(Timeout(5 seconds)).asInstanceOf[Future[String]]
  }


  def receive = {
    case p:(String, T) => {
      myMap += (p._1 -> p._2)
    }

    case "codePillsActor::list" => {
      sender() ! myMap.keySet.mkString("\n")
    }

    case n:String => {
       sender() ! myMap.get(n)
      }
    }
}

