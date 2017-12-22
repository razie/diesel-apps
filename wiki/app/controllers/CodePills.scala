package controllers

import play.api.mvc.{Action, Result}
import com.google.inject.Singleton

import scala.collection.mutable

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

  private val pills = new mutable.HashMap[String, BasePill]()

  /** add a pill by name with the given body */
  def add(name: String)(body: RazRequest => Result): Unit = {
    pills += (name -> new Pill1(name, body))
  }

  /** add a text pill by name with the given body */
  def addString(name: String)(body: RazRequest => String): Unit = {
    pills += (name -> new Pill2(name, body))
  }

  // routes pill/:name?attrs
  def run(pill: String) = Action { implicit request =>
    pill match {
      case _ if pills contains pill => {
        pills(pill).run(razRequest(request))
      }

      case _ => {
        Unauthorized("pill not found")
      }
    }
  }

  // initialize the thing
  add ("list") {request=>
    Ok(pills.keySet.mkString("\n"))
  }

}
