/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.util

import controllers.{IgnoreErrors, VErrors}
import org.bson.types.ObjectId
import play.api.mvc.RequestHeader
import razie.Logging
import razie.wiki.model.{Perm, WikiUser}

/**
 * Authentication: need to provide an authentication service
 *
 *  when authenticated / logged in, a user will store something in the session cookie
 */
trait AuthService[+U <: WikiUser] {
  /** clean the cache for current user - probably a profile change, should reload profile */
  def cleanAuth(u: Option[WikiUser] = None)(implicit request: RequestHeader)

  /** clean the cache for given user - probably a profile change, should reload profile.
    * needed this for cluster auth. State is evil!
    */
  def cleanAuth2(u: WikiUser)

  /** authentication - find the user currently logged in */
  def authUser(implicit request: RequestHeader): Option[U]

  def checkActive(au: WikiUser)(implicit errCollector: VErrors = IgnoreErrors): Option[Boolean]

  /** sign this content - return the signature */
  def sign(content: String): String

  /** check that the signatures match - there's a trick here, heh */
  def checkSignature(realm:String, sign: String, signature: String, user:Option[WikiUser]): Boolean =
    sign == signature
}

/** sample user implementation */
case class MyUser(userName: String, email: String, _id: ObjectId = new ObjectId()) extends WikiUser {
  def ename = userName
  def myPages(realm: String, cat: String): List[Any] = List.empty
  def css = Some("dark") // dark/light preferences
  def hasMembershipLevel(s:String) = false
  def membershipLevel =Perm.Member
  def isActive = true
  def isSuspended = false

  def hasPerm(p: Perm) : Boolean = false
}

/** sample stub authentication */
object NoAuthService extends AuthService[MyUser] with Logging {
  final val harry = MyUser("Harry", "harry@hogwarts.mag")

  /** clean the cache for current user - probably a profile change */
  def cleanAuth(u: Option[WikiUser] = None)(implicit request: RequestHeader) = {
  }

  /** clean the cache for given user - probably a profile change, should reload profile.
    * needed this for cluster auth. State is evil!
    */
  def cleanAuth2(u: WikiUser) = {}

  /** authentication - find the user currently logged in */
  def authUser(implicit request: RequestHeader): Option[MyUser] = {
    Some(harry)
  }

  def checkActive(au: WikiUser)(implicit errCollector: VErrors = IgnoreErrors) = Some(true)

  /** sign this content */
  def sign(content: String): String = "EVERYTHING_GOES" // you MUST IMPLEMENT THIS, heh
}


