/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.model

import org.bson.types.ObjectId
import razie.wiki.util.NoAuthService

/** user permissions */
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

/** basic user concept - you have to provide your own implementation */
abstract class WikiUser {
  def userName: String
  def email: String
  def _id: ObjectId

  def ename: String // make up a nice name: either first name or email or something

  /** pages of category that I linked to. Use wildcard '*' for all realms and all cats */
  def myPages (realm:String, cat: String) : List[Any]

  def css : Option[String] // "dark" vs "light" if there is a preference

  /** check if the user has the given membership level.
    *
    * @param s suggested levels are: Member, Basic, Gold, Platinum
    * @return
    */
  def hasMembershipLevel(s:String) : Boolean

  def hasPerm(p: Perm) : Boolean

  def isActive : Boolean
  def isSuspended : Boolean
  def isMod = isAdmin || hasPerm(Perm.Moderator)
  def isAdmin = hasPerm(Perm.adminDb) || hasPerm(Perm.adminWiki)
}

/** user factory and utils */
trait WikiUsers {
  def findUserById(id: ObjectId) : Option [WikiUser]
  def findUserByUsername(uname: String) : Option [WikiUser]
}

/** sample dummy */
object NoWikiUsers extends WikiUsers {
  def findUserById(id: ObjectId) : Option [WikiUser] = Some(NoAuthService.harry)
  def findUserByUsername(uname: String) : Option [WikiUser] = Some(NoAuthService.harry)
  def isActive = true
}

/** provide implementation in Global::beforeStart() */
object WikiUsers {
  var impl : WikiUsers = NoWikiUsers
}