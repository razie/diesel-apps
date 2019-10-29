/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package controllers

import razie.Logging

/** a correction - meaning an error has a known corrective action
  *
  * easy to use in controllers:
  *
  * ```
  * for(page <- Wikis.find(...) orCorr Corr("can't find wiki", Some("try again"))
  * ```
  */
case class Corr(err: String, action: Option[String] = None) {
  def this(e: String, l: String) = this (e, Some(l))
  override def toString = """Error: """ + err + action.map(" -> " + _).getOrElse("") + ""
  def apply (moreInfo:String) = Corr (s"$err ($moreInfo)", action)
}

/** common validation utilities - utilities and constants for errors/corrections and so on. mix into your controllers to get these
  *
  * this is meant to be mixed into controllers
  * */
trait Validation extends Logging {

  final val cNoAuth = new Corr("Not logged in", """You need to log in or register and then click the link again!"""); //cNoAuth;
  final val cExpired = new Corr("request expired", "login and try again")
  final val cNoProfile = InternalErr("can't load the user profile")
  final def cNoPermission = InternalErr("No permission!")
  final val cNotVerified = new Corr("No permission", """You need to verify your email address!""")
  final val cDemoAccount = new Corr("This is a demo account", """You need to <a href="/do/signout">logout</a> and create your own account!""")
  final val cAccountNotActive = new Corr("This account is not active", """create a <a href="/doe/support?desc=Account+inactive">support request</a> to re-activate!""")

  def cNotMember (clubName:String, role:String="member") = new Corr(
    s"This is a members only topic and you are not a member [Role=$role]")
//    ,  """You need to request membership in this club <a href="/wikie/linkuser/Club:%s?wc=0">%s</a>!""".format(clubName,clubName))

  def cNotAdmin (clubName:String) = new Corr(
      s"This is an admin operation and you are not an admin [$clubName]",
      "")

  def cNoQuotaUpdates = new Corr(
      "You have exceeded your quota for now - awaiting review.",
      """You will be notified when your quota has been restored. If you think something is not right, please send a support request!""".format())

  object InternalErr {
    def apply(err: String) = Corr (err, Some("""create a <a href="/doe/support?desc=Internal+error">suppport request</a>"""))
  }

  def Nope(msg: String) = { debug("ERR Nope: "+msg); None }

  /** the laziness in orErr/orCorr means you can log security or other issues right there - it only calls it when failing
    *
    * this class and implicits below allow stuff like:
    *
    * <code>
    * for (
    *  au <- auth orCorr cNoAuth;  // auts returns an option
    *  hasQuota <- (au.isAdmin || au.quota.canUpdate) orCorr cNoQuotaUpdates;
    *  )
    * <code>
    * */
  case class OptNope[A](o: Option[A]) {

    def logging (msg: String*) (implicit errCollector: VErrors = IgnoreErrors) = { log(msg.mkString(",")); this }
    def orErr   (msg: => String) (implicit errCollector: VErrors = IgnoreErrors): Option[A] = { if (o.isDefined) o else Nope(errCollector.add(msg)) }
    def orCorr  (msg: => Corr) (implicit errCollector: VErrors = IgnoreErrors): Option[A] = { if (o.isDefined) o else Nope(errCollector.add(msg).err) }
    def map[B]  (f: A => B) (implicit errCollector: VErrors = IgnoreErrors): Option[B] = { o map f }
  }

  implicit def toON[A](o: Option[A]) = { OptNope[A](o) }
  implicit def toON2(o: Boolean) = { OptNope(if (o) Some(o) else None) }
  implicit def toC2 (t:(String,String)) = Corr(t._1, Some(t._2))
}

/** collecting errors - the idea is that errors are collected throughout the code and then presented at the end */
class VErrors(var err: List[Corr] = Nil) {
  def hasCorrections = err.foldLeft(false)((a,b)=>a || b.action.isDefined)
  def add(s: Corr) = { err = err ::: s :: Nil; s }
  def add(s: String) = { err = err ::: Corr(s) :: Nil; s }
  def reset { err = Nil }

  def mkString = err.mkString("<br>")
}

/** void error collector */
object IgnoreErrors extends VErrors with Logging {
  override def add (s: Corr) = { debug ("IGNORING error: " + s); s }
  override def add (s: String) = { debug ("IGNORING error: " + s); s }
}


