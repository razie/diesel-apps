/**   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package razie.tconf

import org.bson.types.ObjectId

/** basic user concept - you have to provide your own implementation
  *
  * configurations are normally user-aware, especially when drafting
  *
  * the wiki adds WikiUser and WikiUsers
  *
  * final runtime adds User and Users
  */
trait DUser {
  def userName: String
  def id: String

  def ename: String // make up a nice name: either first name or email or something

  /** user has preferences per realm */
  def realmPrefs (realm:String) = Map.empty[String,String]
}

/** user factory and utils */
trait DUsers[+U <: DUser] {
  def findUserById(id: ObjectId) : Option [U]
  def findUserByUsername(uname: String) : Option [U]
  def findUserByEmailDec(emailDec: String) : Option [U]
}

/** sample dummy */
object NoUsers extends DUsers[DUser] {
  def findUserById(id: ObjectId) : Option [DUser] = None
  def findUserByUsername(uname: String) : Option [DUser] = None
  def findUserByEmailDec(emailDec: String) : Option [DUser] = None
  def isActive = false
}

/** provide implementation in Global::beforeStart() or Module */
object DUsers {
  var impl : DUsers[DUser] = NoUsers
}

