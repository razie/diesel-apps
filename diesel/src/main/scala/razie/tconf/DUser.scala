package razie.tconf

/** basic user concept - you have to provide your own implementation
  *
  * configurations are normally user-aware, especially when drafting
  */
trait DUser {
  def userName: String
  def id: String

  def ename: String // make up a nice name: either first name or email or something
}

