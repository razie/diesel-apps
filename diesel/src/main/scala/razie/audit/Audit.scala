/**   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
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
  * With the Mongo implementation, there are two tables: Audit for events to be reviewed and
  * AuditCleared for reviewed events. You need your own purging
  * of the AuditCleared table.
  *
  * The Play Controller for these is controllers.AdminAudit
  *
  * these are also used for cluster notifications
  *
  * TODO should have a configurable workflow for each of these - what's the pattern?
  */
@RTable
case class Audit(
  level: String,
  msg: String, // audit code - use single constants like a text code
  details: String, // free form
  link: Option[String] = None, // optional link to the entity involved
  node: Option[String] = None, // will populate http of current node
  when: DateTime = DateTime.now,
  _id: ObjectId = new ObjectId
) extends REntity[Audit] {

  // quiet - can't audit the audit entries themselves...
  override def create(implicit txn: Txn = tx.auto) = RCreate.noAudit[Audit](this)
}

/**
  * just a proxy to auditing
  */
object Audit extends SI[AuditService]("Audit", new NoAuditService) with AuditService with Logging {
  // inject yours, in your Module - we use the MdbAuditService but it needs the DB initialized...

  def logdb(what: String, details: Any*) =
    getInstance.logdb(what, details: _*)

  /** log a db operation */
  def logdbWithLink(what: String, link: String, details: Any*) =
    getInstance.logdbWithLink(what, link, details: _*)
}
