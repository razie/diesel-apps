/**   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 **/
package razie.audit

import razie.Log

/**
  * fairly generic website audit service.
  *
  * many operations are audited - this is strait in the log files, as well as the logdb() audit facility.
  * Audited events are supposed to end up somewhere special or even notify someone etc
  *
  * in my RK, I simply log them in a database, for review - you could do the same or ignore them
  *
  * there's a Audit default implementation here somehwer
  */
trait AuditService extends Auditor with razie.Logging {

  /** audit a security issue */
  def security(what: String, details: Any*) =
    logdb("SECURITY_ISSUE", what, details)

  /** audit creation of an entity */
  def create[T](entity: T): T = {
    audit(logdb(ENTITY_CREATE, entity.toString)); entity
    entity
  }
  def createnoaudit[T](entity: T): T = {
    audit(ENTITY_CREATE + " " + entity.toString); entity
  }

  /** audit update of an entity */
  def update[T](entity: T): T = {
    audit(logdb(ENTITY_UPDATE, entity.toString)); entity
  }
  def updatenoaudit[T](entity: T): T = {
    audit(ENTITY_UPDATE + " " + entity.toString); entity
  }

  /** audit delete of an entity */
  def delete[T](entity: T): T = {
    audit(logdb(ENTITY_DELETE, entity.toString)); entity
  }

  final val ENTITY_CREATE = "ENTITY_CREATE"
  final val ENTITY_UPDATE = "ENTITY_UPDATE"
  final val ENTITY_DELETE = "ENTITY_DELETE"

  final val REGD_EMAIL = "INFO_REGD_EMAIL"
  final val ERR_NO_SUCH_USER = "ERR_NO_SUCH_USER"
  final val ERR_FAILEDWIKIEDIT = "ERR_FAILEDWIKIEDIT"
  final val ERR_AUTHREQUIRED = "ERR_AUTHREQUIRED"
  final val ERR_UNAUTHORIZED = "ERR_UNAUTHORIZED"

  private def many(m: Any*) = m.mkString(" ")

  def missingPage(url: String) { error(many("WIKI_MISSING_PAGE", url)) }

  def regdemail(email: String) { audit(logdb(REGD_EMAIL, email)) }

  def wrongLogin(email: String, pwd: String, count:Int) {
    audit(logdb(ERR_NO_SUCH_USER, many(email, pwd, count)))
  }

  def wikiFailedEdit(user: String, cat: String, name: String) {
    audit(logdb(ERR_FAILEDWIKIEDIT, many(user, cat, name)))
  }

  def auth(details: String) { audit(logdb(ERR_AUTHREQUIRED, many(details))) }

  def unauthorized(details: String) {
    audit(logdb(ERR_UNAUTHORIZED, many(details)))
  }
}

/** sample stub audit service - no database backup */
class NoAuditService extends AuditService {

  /** log a db opreation - this method you need to provide */
  def logdb(what: String, details: Any*): String = {
    val d = details.mkString(",")
    val s = what + " " + d
    Log.audit(s)
    s
  }

  /** log a db operation */
  def logdbWithLink(what: String, link: String, details: Any*) = {
    val d = details.mkString(",")
    val s = what + " " + d
    Log.audit(s)
    s
  }
}
