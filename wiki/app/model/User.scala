/**
 *    ____    __    ____  ____  ____,,___     ____  __  __  ____
 *   (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package model

import razie.wiki.Sec._
import com.mongodb.casbah.Imports._
import org.bson.types.ObjectId
import org.joda.time.DateTime
import com.novus.salat._
import com.novus.salat.annotations._
import razie.db.RazSalatContext._
import razie.Log
import razie.wiki.model._
import razie.db._
import razie.db.tx.txn
import razie.audit.Audit
import razie.diesel.dom.WikiDomain
import razie.hosting.WikiReactors
import razie.wiki.util.Maps
import scala.collection.mutable

object UserType {
  val Organization = "Organization"
}

case class Location(city: String, state: String, country: String) {}

/** a person that may or may not have an account - or user group */
trait TPersonInfo {
  def firstName: String
  def lastName: String
  def email: String
  def emailDec: String
  def yob: Int
  def gender: String // M/F/?
  def roles: Set[String]
  def status: Char
  def notifyParent: Boolean
  def _id: ObjectId

  def ename = if (firstName != null && firstName.size > 0) firstName else emailDec.replaceAll("@.*", "")
  def fullName = firstName + " " + lastName
  def role = roles.head

  def organization:Option[String]
}

/** account - Minimal user info - loaded all the time for a user */
@RTable
case class User(
  userName: String,
  firstName: String,
  lastName: String,
  yob: Int,
  email: String,   // encrypted
  pwd: String,     // encrypted

  // this is per realm
  status: Char = Users.ACTIVE, // a-active, s-suspended, d-deleted
  roles: Set[String], // = Set("Racer"),

  realms: Set[String]=Set(), // = RK modules (notes, rk, ski etc)
  addr: Option[String] = None, // address as typed in
  prefs: Map[String, String] = Map(), // user preferences
  gid: Option[String] = None, // google id
  modNotes: Seq[String] = Seq.empty, // google id
  clubSettings : Option[String] = None, // if it's a club - settings

  organization : Option[String] = None, // possibly name of org

  perms: Set[String] = Set(),

  realmSet: Map[String, UserRealm] = Map(), // per realm
  consent:Option[String] = None,
  apiKey:Option[String] = Some(new ObjectId().toString),

  _id: ObjectId = new ObjectId()) extends WikiUser with TPersonInfo {

  def emailDec = email.dec

  def gender = "?"
  def notifyParent = false

  override def ename = if (firstName != null && firstName.size > 0) firstName else emailDec.replaceAll("@.*", "")

  def tasks = Users.findTasks(_id)

  override def isActive = status == 'a'
  override def isSuspended = status == 's'
  //def isMod = isAdmin || hasPerm(Perm.Moderator)
  def isClub = roles contains UserType.Organization.toString
  def isUnder13 = DateTime.now.year.get - yob <= 12

  /** is this a group admin for a group the page belongs to ? */
  def canAdmin (we:WikiEntry) : Boolean = isAdmin //||
//    we.wid.parentOf(WikiDomain(we.realm).isA("Club", _)).flatMap(Club.apply).exists(_.isClubAdmin(this))

  /** is this a group admin for a group the page belongs to ? */
  def canAdmin (wid:WID) : Boolean = isAdmin || wid.page.exists(canAdmin)

  /** Harry is a default suspended account you can use for demos */
  def isHarry = id == "4fdb5d410cf247dd26c2a784"

  // TODO optimize
  def allPerms: Set[String] = perms ++ groups.flatMap(_.can)
  def hasPerm(p: Perm) = allPerms.contains("+" + p.s) && !allPerms.contains("-" + p.s)

  override def membershipLevel : String =
      if(this.hasPerm(Perm.Moderator) || this.isAdmin) Perm.Moderator.s
      else if (this.hasPerm(Perm.Unobtanium)) Perm.Unobtanium.s
      else if (this.hasPerm(Perm.Platinum)) Perm.Platinum.s
      else if (this.hasPerm(Perm.Gold)) Perm.Gold.s
      else if (this.hasPerm(Perm.Basic)) Perm.Basic.s
      else Perm.Member.s

  override def hasMembershipLevel(s:String) =
    (s == Perm.Member.s) ||
    (s == Perm.Moderator.s && (this.hasPerm(Perm.Moderator) || this.isAdmin)) ||
    (s == Perm.Unobtanium.s && (this.hasPerm(Perm.Moderator) || this.hasPerm(Perm.Unobtanium))) ||
    (s == Perm.Platinum.s && (this.hasPerm(Perm.Moderator) || this.hasPerm(Perm.Unobtanium) || this.hasPerm(Perm.Platinum))) ||
    (s == Perm.Gold.s && (this.hasPerm(Perm.Moderator) || this.hasPerm(Perm.Unobtanium) || this.hasPerm(Perm.Platinum) || this.hasPerm(Perm.Gold))) ||
    (s == Perm.Basic.s && (this.hasPerm(Perm.Moderator) || this.hasPerm(Perm.Unobtanium) || this.hasPerm(Perm.Platinum) || this.hasPerm(Perm.Gold) || this.hasPerm(Perm.Basic)))

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
  lazy val clubs =
    wikis.filter(uw=>WikiDomain(uw.uwid.getRealm).isA("Club", uw.uwid.cat)).toList

  // perf issue - checked for each user logged in
  def isLinkedTo(uwid:UWID) =
    wikis.filter(uw=>uw.uwid.cat==uwid.cat && uw.uwid.id == uwid.id)

  /** pages of category that I linked to */
  def pages(realm:String, cat: String*) = wikis.filter{w=>
    (cat=="*"   || cat.contains(w.uwid.cat)) &&
    (realm=="*" || realm==w.uwid.getRealm || WikiReactors(realm).supers.contains(w.uwid.getRealm))
  }

  def myPages(realm:String, cat: String) = pages (realm, cat)

  lazy val memberReactors = (ownedPages("rk", "Reactor").map(_.name).toList ::: realms.toList).distinct

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

  def update(newu: User) = {
    RazMongo("UserOld") += grater[User].asDBObject(Audit.create(this))
    RazMongo("User").update(key, grater[User].asDBObject(Audit.update(newu)))
    UserEvent(_id, "UPDATE").create
  }

  def usedSlot(u: User) = {
    RazMongo("UserOld") += grater[User].asDBObject(Audit.create(this))
    RazMongo("User").update(key, grater[User].asDBObject(Audit.update(u)))
    UserEvent(_id, "UPDATE").create
  }

  def toJson = {
    grater[User].asDBObject(this).toString
  }

  def shouldEmailParent(what: String) = {
    if (isUnder13) {
      for (
        pc <- Users.findParentOf(this._id) if (pc.notifys == what);
        parent <- ROne[User](pc.parentId)
      ) yield parent
    } else None
  }

  def hasRealm(m:String) = realms.contains(m) || (realms.isEmpty && Wikis.RK == m)

  def quota = ROne[UserQuota]("userId" -> _id) getOrElse UserQuota(_id)

  var css = prefs.get("css")

  def mapRS(realm:String)(f: UserRealm => UserRealm) = {
    // if not there, create record for this realm
    val x = if(realmSet.contains(realm) || "*" == realm)
      this
    else {
      this.copy(
        realmSet =
          this.realmSet ++
          Seq((realm -> UserRealm( status, roles, perms, prefs - "dieselEnv", modNotes, consent )))
        // don't copy the dieselEnv
      )
    }

    x.copy(
        realmSet = x.realmSet.map { rs =>
        if (rs._1 == realm || "*" == realm) (rs._1, f(rs._2))
        else rs
      })
  }

  def consented(realm:String, ver: String) = mapRS(realm) {rs=>
    rs.copy(consent = Some(ver + " on " +DateTime.now().toString()))
  }

  def hasConsent(realm:String) =
    this.consent.isDefined ||
      this.realmSet.get(realm).orElse(this.realmSet.get("rk")).exists(_.consent.isDefined)

  def clearConsent(realm:String) = mapRS(realm) {rs=>
    rs.copy(consent = None)
  }

  def addPerm(realm:String, t: String) = mapRS(realm) {rs=>
    rs.copy(perms = rs.perms + (if(t.startsWith("+")) t else "+"+t))
  }

  def removePerm(realm:String, t: String) = mapRS(realm) {rs=>
    rs.copy(perms = rs.perms - t)
  }

  def forRealm(realm:String) = {
    // default to rk for backwards comp for old users wihtout realms
    this.realmSet.get(realm).orElse(this.realmSet.get("rk")).map{rs=>
      this.copy(
        status = rs.status,
        roles = rs.roles,
        prefs = rs.prefs,
        perms = rs.perms,
        modNotes = rs.modNotes,
        consent = rs.consent
      )
    }.getOrElse(this)
  }

  def getPrefs(name: String, default:String="") = prefs.get(name).getOrElse(default)
  def setPrefs(realm:String, p:Map[String,String]) = mapRS(realm) {rs=>
    rs.copy(prefs = rs.prefs ++ p)
  }

  override def realmPrefs(realm:String) =
    realmSet.get(realm).map(_.prefs).getOrElse(Map.empty)

  def setRoles(realm:String, s: String) = mapRS(realm) {rs=>
    rs.copy(roles = s.split("[, ]").toSet)
  }

  def setRoles(realm:String, s: Set[String]) = mapRS(realm) {rs=>
    rs.copy(roles = s)
  }

  def setRealms(realm:String, s: String) = {
    this.copy(realms = s.split("[, ]").toSet)
  }

  def setStatus(realm:String, s: String) = mapRS(realm) {rs=>
    rs.copy(status = s(0))
  }

  def addModNote(realm:String, note: String) = mapRS(realm) {rs=>
    rs.copy(modNotes = rs.modNotes ++ Seq(note))
  }

  def removeModNote(realm:String, note: String) = mapRS(realm) {rs=>
    rs.copy(modNotes = rs.modNotes.filter(_ != note))
  }

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
  aboutMe: Option[WikiEntry] = None,
  relationships: Map[String, String] = Map(), // (who -> what)
  contact: Option[Contact] = None,

  newExtLinks : Seq[ExtSystemUserLink]= Seq(),

  _id: ObjectId = new ObjectId()) {

  def update(p: Profile) =  RUpdate(Map("userId" -> userId), p)

  def addRel(t: (String, String)) = this.copy(relationships = relationships ++ Map(t))
  def addTag(t: String) = this.copy(tags = tags + t)
  def setContact(c: Contact) = this.copy(contact = Some(c))

  def toJson = grater[Profile].asDBObject(this).toString

  var createdDtm: DateTime = DateTime.now
  var lastUpdatedDtm: DateTime = DateTime.now

  def getExtLink (realm:String, systemId:String, instanceId:String) =
    newExtLinks.find(x=> x.realm == realm && x.extSystemId == systemId && x.extInstanceId == instanceId)

  /** add or update existing link */
  def upsertExtLink (realm:String, link:ExtSystemUserLink) = {
    val e1 = newExtLinks.filter(x=> !( x.realm == realm && x.extSystemId == link.extSystemId && x.extInstanceId == link.extInstanceId))
    val e2 = e1 ++ Seq(link)
    this.copy(newExtLinks = e2)
  }
}

/** link to an external system account */
case class ExtSystemUserLink (
  realm:String,
  extSystemId : String,    // external system id
  extInstanceId : String,  // external system id
  extAccountId : String    // external account id
  )

/** settings per realm */
case class UserRealm (
  status: Char = Users.ACTIVE, // a-active, s-suspended, d-deleted
  roles: Set[String], // like groups perms etc

  perms: Set[String] = Set(),

  prefs: Map[String, String] = Map(), // user preferences
  modNotes: Seq[String] = Seq.empty,
  consent:Option[String] = None
  ) {
  }
