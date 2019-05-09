/**
  *   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 **/
package razie.audit

import org.bson.types.ObjectId
import org.joda.time.DateTime
import razie.Logging
import razie.db._

/**
  * generic audit event - stored in db table, log or ELK etc.
  *
  * Some of these may end up as emails or alerts.
  *
  * With Mongo implementation, there are two tables: Audit for events to be reviewed and
  * AuditCleared for reviewed events. You need your own purging
  * of the AuditCleared table.
  *
  * these are also used for cluster notifications
  *
  * TODO should have a configurable workflow for each of these - what's the pattern?
  */
@RTable
case class Audit(
  level: String,
  msg: String, // free format message
  details: String, // free form
  link: Option[String] = None, // optional link to the entity involved
  node: Option[String] = None, // will populate http of current node
  when: DateTime = DateTime.now,
  _id: ObjectId = new ObjectId
) extends REntity[Audit] {
  // quiet - can't audit the audit entries
  override def create(implicit txn: Txn = tx.auto) =
    RCreate.noAudit[Audit](this)
}

/**
  * just a proxy to auditing
  */
object Audit extends AuditService with Logging {
  // default NEEDS to be dumb and is changed in Module
  var impl: AuditService = new NoAuditService

  def logdb(what: String, details: Any*) =
    impl.logdb(what, details: _*)

  /** log a db operation */
  def logdbWithLink(what: String, link: String, details: Any*) =
    impl.logdbWithLink(what, link, details: _*)
}
