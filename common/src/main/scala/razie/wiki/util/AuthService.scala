/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.util

import org.bson.types.ObjectId
import play.api.mvc.Request
import razie.Logging
import razie.wiki.model.WikiUser

/**
 * need to provide an authentication service
 *
 *  when authenticated / logged in, a user will store something in the session cookie
 */
trait AuthService[+U <: WikiUser] {
  /** clean the cache for current user - probably a profile change, should reload profile */
  def cleanAuth(u: Option[WikiUser] = None)(implicit request: Request[_])

  /** authentication - find the user currently logged in */
  def authUser(implicit request: Request[_]): Option[U]

  def checkActive(au: WikiUser)(implicit errCollector: VErrors = IgnoreErrors): Option[Boolean]

  /** sign this content - return the signature */
  def sign(content: String): String

  /** check that the signatures match - there's a trick here, heh */
  def checkSignature(sign: String, signature: String): Boolean
}

/** sample user implementation */
case class MyUser(userName: String, email: String, _id: ObjectId = new ObjectId()) extends WikiUser {
  def ename = userName
  def myPages(realm: String, cat: String): List[Any] = List.empty
}

/** sample stub authentication */
object NoAuthService extends AuthService[MyUser] with Logging {
  val harry = MyUser("Harry", "harry@hogwarts.mag")

  /** clean the cache for current user - probably a profile change */
  def cleanAuth(u: Option[WikiUser] = None)(implicit request: Request[_]) {
  }

  /** authentication - find the user currently logged in */
  def authUser(implicit request: Request[_]): Option[MyUser] = {
    Some(harry)
  }

  def checkActive(au: WikiUser)(implicit errCollector: VErrors = IgnoreErrors) = Some(true)

  /** sign this content */
  def sign(content: String): String = "EVERYTHING_GOES" // you MUST IMPLEMENT THIS, heh

  /** check that the signatures match - there's a trick here, heh */
  def checkSignature(sign: String, signature: String): Boolean = sign == signature
}


