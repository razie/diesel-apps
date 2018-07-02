/**
 *    ____    __    ____  ____  ____,,___     ____  __  __  ____
 *   (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package model

import com.mongodb.DBObject
import com.mongodb.casbah.Imports._
import com.mongodb.util.JSON
import com.novus.salat._
import org.bson.types.ObjectId
import org.joda.time.DateTime
import razie.audit.Audit
import razie.db.RazSalatContext._
import razie.db._
import razie.db.tx.txn
import razie.wiki.Sec._
import razie.wiki.{Enc, Services}
import razie.wiki.model._

/** permissions for a user group */
@RTable
case class UserGroup(
  name: String,
  can: Set[String] = Set(Perm.uProfile.plus),
  _id : ObjectId = new ObjectId()
  ) extends REntity[UserGroup]


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

/** cqrs decouplig */
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

  lazy val user = ROne[User](userId)
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

  final val SUSPENDED = 's'
  final val ACTIVE = 'a'
  final val DELETED = 'd'

  final val ROLE_MEMBER = "Member"

  def fromJson(j: String) = Option(grater[User].asObject(JSON.parse(j).asInstanceOf[DBObject]))

  /** find user by lowercase email - at loging */
  def findUserNoCase(uncEmail: String) = {
    // todo optimize somwhow
    val tl = uncEmail.toLowerCase()
    RazMongo("User") findAll() filter(_.containsField("email")) find (x=>x.as[String]("email") != null && x.as[String]("email").dec.toLowerCase == tl) map (grater[User].asObject(_))
  }

  /** find user by lowercase email - at loging */
  def findUsersForRealm(realm: String) = {
    // todo optimize somwhow
    RazMongo("User") findAll() filter(x=>
      realm == "*" ||
        x.containsField("realms") &&
          x.as[Seq[String]]("realms") != null &&
          x.as[Seq[String]]("realms").contains(realm)
      ) map (
        grater[User].asObject(_)
      ) filter (u=>
        realm == "*" || (u.realms contains realm)
      )
  }

  /** find by decrypted email */
  def findUserByEmailDec(emailDec: String) = findUserByEmailEnc(Enc(emailDec))
  /** find by encrypted email */
  def findUserByEmailEnc(emailEnc: String) = ROne[User]("email" -> emailEnc)

  def findUserById(id: String) = ROne[User](new ObjectId(id))
  def findUserById(id: ObjectId) = ROne[User](id)
  def findUserByUsername(uname: String) = ROne[User]("userName" -> uname)

  import play.api.Play.current

  //todo optimize this - cache some users?
  /** display name of user with id, for comments etc */
  def nameOf(uid: ObjectId): String = {
    Cache.getAs[String](uid.toString + ".username").getOrElse {
      val n = ROne.raw[User]("_id" -> uid).fold("???")(_.apply("userName").toString)
      Cache.set(uid.toString + ".username", n, 600) // 10 miuntes
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

/** represents a unique user id */
case class UserId (id:String)

