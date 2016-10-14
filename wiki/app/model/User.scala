/**
 *    ____    __    ____  ____  ____,,___     ____  __  __  ____
 *   (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package model

import razie.base.Audit
import razie.wiki.Sec._
import com.mongodb.casbah.Imports._
import com.mongodb.util.JSON
import org.bson.types.ObjectId
import org.joda.time.DateTime
import com.novus.salat._
import com.novus.salat.annotations._
import razie.db.RazSalatContext._
import java.net.URLEncoder
import razie.Log
import controllers.Maps
import razie.wiki.Services
import scala.annotation.StaticAnnotation
import razie.wiki.model._
import razie.wiki.admin.MailSession
import com.mongodb.DBObject
import razie.db._
import razie.Snakk
import razie.db.RMongo.as
import razie.db.tx.txn

import scala.collection.mutable

object UserType {
  val Organization = "Organization"
}

/** external user for following stuff */
@RTable
case class Follower(
  email: String,
  name: String,
  when: DateTime = DateTime.now,
  _id: ObjectId = new ObjectId()) extends REntity[Follower]

/** external user for following stuff */
@RTable
case class FollowerWiki(
  followerId: ObjectId,
  comment: String,
  uwid: UWID,
  _id: ObjectId = new ObjectId()) {
  def delete = { Audit.delete(this); RDelete[FollowerWiki]("_id" -> _id) }

  def follower = ROne[Follower](followerId)
}

/** permissions for a user group */
@RTable
case class UserGroup(
  name: String,
  can: Set[String] = Set(Perm.uProfile.plus),
  _id : ObjectId = new ObjectId()
  ) extends REntity[UserGroup]

case class Location(city: String, state: String, country: String) {}

case class Perm(s: String) {
  def plus = "+" + this.s
  def minus = "-" + this.s
}

/** permissions */
object Perm {
  val adminDb = Perm("adminDb") // god - can fix users etc
  val adminWiki = Perm("adminWiki") // can administer wiki - edit categories/reserved pages etc
  val uWiki = Perm("uWiki") // can update wiki
  val uProfile = Perm("uProfile")
  val eVerified = Perm("eVerified")
  val apiCall = Perm("apiCall") // special users that can make api calls
  val domFiddle = Perm("domFiddle") // can create services in eithe scala or JS
  val codeMaster = Perm("codeMaster") // can create services in eithe scala or JS

  // membership level (paid etc)
  val Member = Perm("Member") // not paid account - this is not actually needed in the profile - if au then member
  val Basic = Perm("Basic") // paid account
  val Gold = Perm("Gold") // paid account
  val Platinum = Perm("Platinum") // paid account
  val Moderator = Perm("Moderator") // paid account

  implicit def tos(p: Perm): String = p.s

  val all: Seq[String] = Seq(adminDb, adminWiki, uWiki, uProfile, eVerified, apiCall, codeMaster,
    "cCategory", "uCategory", "uReserved",
    Basic, Gold, Platinum, Moderator, domFiddle
  )
}

/** a person that may or may not have an account - or user group */
trait TPersonInfo {
  def firstName: String
  def lastName: String
  def email: String
  def yob: Int
  def gender: String // M/F/?
  def roles: Set[String]
  def status: Char
  def notifyParent: Boolean
  def _id: ObjectId

  def ename = if (firstName != null && firstName.size > 0) firstName else email.dec.replaceAll("@.*", "")
  def fullName = firstName + " " + lastName
  def role = roles.head
}

/** account - Minimal user info - loaded all the time for a user */
@RTable
case class User(
  userName: String,
  firstName: String,
  lastName: String,
  yob: Int,
  email: String,
  pwd: String,
  status: Char = 'a', // a-active, s-suspended, d-deleted
  roles: Set[String], // = Set("Racer"),
  realms: Set[String]=Set(), // = RK modules (notes, rk, ski etc)
  addr: Option[String] = None, // address as typed in
  prefs: Map[String, String] = Map(), // user preferences
  gid: Option[String] = None, // google id
  modNotes: Seq[String] = Seq.empty, // google id
  clubSettings : Option[String] = None, // if it's a club - settings
  _id: ObjectId = new ObjectId()) extends WikiUser with TPersonInfo {

  // TODO change id = it shows like everywhere
  def id = _id.toString
  def gender = "?"
  def notifyParent = false

  override def ename = if (firstName != null && firstName.size > 0) firstName else email.dec.replaceAll("@.*", "")

  def tasks = Users.findTasks(_id)

  def isActive = status == 'a'
  def isSuspended = status == 's'
  def isAdmin = hasPerm(Perm.adminDb) || hasPerm(Perm.adminWiki)
  def isClub = roles contains UserType.Organization.toString
  def isUnder13 = DateTime.now.year.get - yob <= 12

  /** Harry is a default suspended account you can use for demos */
  def isHarry = id == "4fdb5d410cf247dd26c2a784"

  // TODO optimize
  def perms: Set[String] = profile.map(_.perms).getOrElse(Set()) ++ groups.flatMap(_.can).toSet
  def hasPerm(p: Perm) = perms.contains("+" + p.s) && !perms.contains("-" + p.s)

  override def hasMembershipLevel(s:String) =
    (s == Perm.Member.s) ||
    (s == Perm.Moderator.s && (this.hasPerm(Perm.Moderator) || this.isAdmin)) ||
    (s == Perm.Platinum.s && (this.hasPerm(Perm.Moderator) || this.hasPerm(Perm.Platinum))) ||
    (s == Perm.Gold.s && (this.hasPerm(Perm.Moderator) || this.hasPerm(Perm.Platinum) || this.hasPerm(Perm.Gold))) ||
    (s == Perm.Basic.s && (this.hasPerm(Perm.Moderator) || this.hasPerm(Perm.Platinum) || this.hasPerm(Perm.Gold) || this.hasPerm(Perm.Basic)))

  // centered on Toronto by default
  lazy val ll = addr.flatMap(Maps.latlong _).getOrElse(("43.664395", "-79.376907"))

  /** can move around without restrictions */
  lazy val canHasProfile = (!isUnder13) || Users.findParentOf(_id).exists(_.trust == "Public")

  // TODO cache groups
  lazy val groups = roles flatMap(role=> ROne[UserGroup]("name" -> role))

  /** make a default profile */
  def mkProfile = Profile(this._id, None, Set())

  /** relationships */
  def rel(r: String): List[User] = profile.map(p =>
    (for (t <- p.relationships if (t._2 == r))
      yield t._1).flatMap(Users.findUserById(_)).toList) getOrElse List()

  /** load my profile */
  lazy val profile = ROne[Profile]("userId" -> _id)

  /** the wikis I linked to */
  lazy val wikis = RMany[UserWiki]("userId" -> _id).toList
//  lazy val clubs = RMany[UserWiki]("userId" -> _id.filter(), "uwid.cat" -> "Club").toList
  lazy val clubs = RMany[UserWiki]("userId" -> _id).filter(uw=>Wikis.domain(uw.uwid.getRealm).isA("Club", uw.uwid.cat)).toList

  def isLinkedTo(uwid:UWID) = (ROne[UserWiki]("uwid" -> uwid.grated, "userId" -> _id) orElse ROne[UserWiki]("uwid.cat" -> uwid.cat, "uwid.id" -> uwid.id, "userId" -> _id)).toList

  /** pages of category that I linked to */
  def pages(realm:String, cat: String*) = wikis.filter{w=>
    (cat=="*"   || cat.contains(w.uwid.cat)) &&
    (realm=="*" || realm==w.uwid.getRealm || WikiReactors(realm).supers.contains(w.uwid.getRealm))
  }

  def myPages(realm:String, cat: String) = pages (realm, cat)

  def ownedPages(realm:String, cat: String) =
    Wikis(realm).weTable(cat).find(Map("props.owner" -> id, "category" -> cat)) map {o=>
      WID(cat, o.getAs[String]("name").get).r(o.getAs[String]("realm").get)
    }

  def auditCreated(realm:String) { Log.audit("USER_CREATED " + email + " realm: " + realm) }
  def auditLogout(realm:String) { Log.audit("USER_LOGOUT " + email+" realm: "+realm) }
  def auditLogin(realm:String) { Log.audit("USER_LOGIN " + email + " realm: " + realm) }
  def auditLoginFailed(realm:String) { Log.audit("USER_LOGIN_FAILED " + email + " realm: " + realm) }

  lazy val key = Map("email" -> email)

  def create(p: Profile) {
    var res = RCreate(this)

    p.createdDtm = DateTime.now()
    p.lastUpdatedDtm = DateTime.now()
    res = RCreate(p)

    UserEvent(_id, "CREATE").create
  }

  def update(u: User) = {
    RazMongo("UserOld") += grater[User].asDBObject(Audit.create(this))
    RazMongo("User").update(key, grater[User].asDBObject(Audit.update(u)))
    UserEvent(_id, "UPDATE").create
  }

  def usedSlot(u: User) = {
    RazMongo("UserOld") += grater[User].asDBObject(Audit.create(this))
    RazMongo("User").update(key, grater[User].asDBObject(Audit.update(u)))
    UserEvent(_id, "UPDATE").create
  }

  def toJson = grater[User].asDBObject(this).toString

  def shouldEmailParent(what: String) = {
    if (isUnder13) {
      for (
        pc <- Users.findParentOf(this._id) if (pc.notifys == what);
        parent <- ROne[User](pc.parentId)
      ) yield parent
    } else None
  }

  def getPrefs(name: String, default:String="") = prefs.get(name).getOrElse(default)
  def setPrefs(name: String, value : => String) = this.update(this.copy(prefs = this.prefs + (name -> value)))

  def hasRealm(m:String) = realms.contains(m) || (realms.isEmpty && Wikis.RK == m)

  def quota = ROne[UserQuota]("userId" -> _id) getOrElse UserQuota(_id)

  var css = prefs.get("css")
}

case class Contact(info: Map[String, String])

/**
 * detailed user profile
 *
 *  perms are permissions
 */
@RTable
case class Profile(
  userId: ObjectId,
  loc: Option[Location] = None, // address as refined with Google
  tags: Set[String] = Set(),
  perms: Set[String] = Set(),
  aboutMe: Option[WikiEntry] = None,
  relationships: Map[String, String] = Map(), // (who -> what)
  contact: Option[Contact] = None,
  consent:Option[String] = None,
  realmInfo: Map[String, String] = Map(), // (who -> what)
  _id: ObjectId = new ObjectId()) {

  def update(p: Profile) =  RUpdate(Map("userId" -> userId), p)

  def addRel(t: (String, String)) = this.copy(relationships = relationships ++ Map(t))
  def addPerm(t: String) = this.copy(perms = perms + t)
  def removePerm(t: String) = this.copy(perms = perms - t)
  def addTag(t: String) = this.copy(tags = tags + t)
  def setContact(c: Contact) = this.copy(contact = Some(c))

  def consented(ver: String) = this.copy(consent = Some(ver + " on " +DateTime.now().toString()))

  def setRealmProp(realm: String, prop:String, value:String) = {
    this.update(this.copy(realmInfo = this.realmInfo + ((realm+"."+prop) -> value)))
  }

  def getRealmProp(realm: String, prop:String, dfltValue:String="") = {
    realmInfo.getOrElse(realm+"."+prop, dfltValue)
  }

  var createdDtm: DateTime = DateTime.now
  var lastUpdatedDtm: DateTime = DateTime.now
}

/**
 * high churn table - user edit slots
 *
 * I use these mainly to limit new users to just a few changes,
 * in case some paparazzi gets through...
 */
@RTable
case class UserQuota(
  userId: ObjectId,
  updates: Option[Int] = Some(5), // number of wikis updated
  _id: ObjectId = new ObjectId()) {

  //  def create = Mongo ("Profile") += grater[Profile].asDBObject(Audit.create(this))
  def update(q: UserQuota) =
    ROne[UserQuota]("userId" -> userId) map { p =>
      RUpdate.noAudit[UserQuota](Map("userId" -> userId), q)
      q
    } getOrElse {
      RCreate.noAudit[UserQuota](q)
      q
    }

  def canUpdate = {
    this.updates.exists(_ > 0) || this.updates.isEmpty
  }

  def incUpdates = {
    val q = this.copy(updates = updates.map(_ - 1) orElse Some(5))
    update(q)

    if (q.updates.exists(_ < 20))
      Services ! EventNeedsQuota(
        Users.findUserById(userId).map(u=> s"$u.userName - $u.firstName $u.lastName").toString,
        userId.toString)
  }

  // admin op
  def reset(i: Int) = {
    val q = this.copy(updates = Some(i))
    update(q)
  }
}

// cqrs decouplig
case class EventNeedsQuota(s1:String, s2:String, node:String="") extends WikiEventBase

/**
 * a parent/child relationship with additional permissions.
 *
 * used to supervise users under 13
 */
@RTable
case class ParentChild(
  parentId: ObjectId,
  childId: ObjectId,
  trust: String = "Private",
  notifys: String = "Everything",
  _id: ObjectId = new ObjectId()) extends REntity[ParentChild] {

  def update(p: ParentChild) = {
    RUpdate(
      Map("parentId" -> parentId, "childId" -> childId),
      p)
  }
  def delete = RDelete[ParentChild]("parentId" -> parentId, "childId" -> childId)
}

object UW {
  final val NOEMAIL = "n"
  final val EMAIL_EACH = "e"
  final val EMAIL_DAILY = "d" // TODO
}

/** a link between a user and a wiki.
  *
  * indicates owner, follower, member etc
  */
@RTable
case class UserWiki(
    userId: ObjectId,
    uwid: UWID,
    role: String,
    notif:String = UW.EMAIL_EACH,
    _id: ObjectId = new ObjectId()) extends REntity[UserWiki] {

  def updateRole(newRole: String) = {
      this.copy(role=newRole).update
  }

  def wlink = WikiLink(UWID("User", userId), uwid, "")
  def wname = wlink.wname

  def user = ROne[User](userId)
  def wid = uwid.wid.get
}

/** the presence of this indicates the user is not verified */
case class UserVerifReq(id: UserId, verifType: String)

/** some user audit info */
@RTable
case class UserEvent(
  userId: ObjectId,
  what: String,
  when: DateTime = DateTime.now()) {

  def create = RCreate(this)
}

import play.api.cache._

/** user factory and utils */
object Users {

  def fromJson(j: String) = Option(grater[User].asObject(JSON.parse(j).asInstanceOf[DBObject]))

  /** find user by lowercase email - at loging */
  def findUserNoCase(uncEmail: String) = {
    // todo optimize somwhow
    val tl = uncEmail.toLowerCase()
    RazMongo("User") findAll() filter(_.containsField("email")) find (x=>x.as[String]("email") != null && x.as[String]("email").dec.toLowerCase == tl) map (grater[User].asObject(_))
  }

  /** you better encrypt before calling */
  def findUserByEmail(email: String) = ROne[User]("email" -> email)
  def findUserById(id: String) = ROne[User](new ObjectId(id))
  def findUserById(id: ObjectId) = ROne[User](id)
  def findUserByUsername(uname: String) = ROne[User]("userName" -> uname)

  import play.api.Play.current

  //todo optimize this - cache some users?
  /** display name of user with id, for comments etc */
  def nameOf(uid: ObjectId): String = {
    Cache.getAs[String](uid.toString + ".name").getOrElse {
      val n = ROne.raw[User]("_id" -> uid).fold("???")(_.apply("userName").toString)
      Cache.set(uid.toString + ".name", n, 600) // 10 miuntes
      n
    }
  }

  /** find tasks for user */
  def findTasks(uid: ObjectId) = {
    val x = RMany[UserTask]("userId" -> uid).toList
//    if(ROne[VolunteerH]("approver" -> findUserById(uid).map(_.email.dec), "approvedBy" -> None).isDefined)
//      UserTasks.approveVolunteerHours(uid) :: x
//    else
      x
  }

  def findPC(pid: ObjectId, cid: ObjectId) = ROne[ParentChild]("parentId" -> pid, "childId" -> cid)
  def findParentOf(cid: ObjectId) = ROne[ParentChild]("childId" -> cid)

  def findUserLinksTo(u: UWID) = RMany[UserWiki]("uwid.cat" -> u.cat, "uwid.id" -> u.id)

  def create(r: Task) = RCreate(r)

  def findFollowerByEmail(email: String) = ROne[Follower]("email" -> email)
  def findFollowerLinksTo(u: UWID) = RMany[FollowerWiki]("uwid.cat" -> u.cat, "uwid.id" -> u.id)
}

/** user factory and utils */
object WikiUsersImpl extends WikiUsers {
  def findUserById(id: String) = Users.findUserById (id)
  def findUserById(id: ObjectId) = Users.findUserById (id)
  def findUserByUsername(uname: String) = Users.findUserByUsername(uname)
}

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

/** represents a unique user id */
case class UserId (id:String)

