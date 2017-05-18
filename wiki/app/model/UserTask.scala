/**
 *    ____    __    ____  ____  ____,,___     ____  __  __  ____
 *   (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package model

import org.bson.types.ObjectId
import org.joda.time.DateTime
import razie.audit.Audit
import razie.db._

/** the static tasks - reference table */
@RTable
case class Task(name: String, desc: String)

@RTable
case class UserTask(
    userId: ObjectId,
    name: String,
    args:Map[String,String] = Map(),
    crDtm:DateTime = DateTime.now) {

  def desc = ROne[Task]("name" -> name) map (_.desc) getOrElse UserTasks.labelFor(this)
  def create (implicit txn:Txn) = RCreate(this)

  def delete (implicit txn:Txn) = {
    Audit.delete(this); // should delete more than one
    RDelete[UserTask]("userId" -> userId, "name" -> name)
  }
}

object UserTasks {
  final val START_REGISTRATION = "startRegistration"
  final val APPROVE_VOL = "approveVolunteerHours"

  def userNameChgDenied(u: User) = UserTask(u._id, "userNameChgDenied")
  def changePass(u: User) = UserTask(u._id, "changePass")
  def verifyEmail(u: User) = UserTask(u._id, "verifyEmail")
  def addParent(u: User) = UserTask(u._id, "addParent")
  def chooseTheme(u: User) = UserTask(u._id, "chooseTheme")
  def setupRegistration(u: User) = UserTask(u._id, "setupRegistration")
  def setupCalendars(u: User) = UserTask(u._id, "setupCalendars")
  def approveVolunteerHours(uid: ObjectId) = UserTask(uid, APPROVE_VOL)

  def some(u: User, what:String) = UserTask(u._id, what)

  def labelFor (ut:UserTask) = {
    ut.name match {
      case START_REGISTRATION => "Start registration for "+ut.args.get("club").mkString
      case "setupRegistration" => "Setup registration and forms"
      case "setupCalendars" => "Setup club calendars"
      case APPROVE_VOL => "Approve volunteer hours"
      case _ => "?"
    }
  }

}


