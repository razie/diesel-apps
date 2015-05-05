/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.admin

import org.bson.types.ObjectId
import org.joda.time.DateTime
import razie.Logging
import razie.db._
import razie.wiki.Services

/**
 *  all audit events - stored in db table. Some of these may end up as emails or alerts.
 *
 *  there are two tables: Audit for events to be reviewed and AuditCleared for reviewed events. You need your own purging
 *  of the AuditCleared table.
 *
 *  TODO should have a configurable workflow for each of these - what's the pattern?
 */
@RTable
case class Audit(
  level: String,
  msg: String,
  details: String,
  link: Option[String] = None,
  when: DateTime = DateTime.now,
  _id: ObjectId = new ObjectId) extends REntity[Audit] {
  override def create (implicit txn: Txn = tx.auto) = RCreate.noAudit[Audit](this)
}

/**
 * just a proxy to
 */
object Audit extends AuditService with Logging {

  /**
   * process some audit objects persisence asynchronously...
   *
   *   TODO I really ended up using this as a generic persistence deferring service...
   */
  def !(a: Any) { Services.alli ! a }
  def !?(a: Any) { Services.alli !? a }

  def logdb(what: String, details: Any*) =
    Services.audit.logdb(what, details:_*)

  /** log a db operation */
  def logdbWithLink(what: String, link: String, details: Any*) =
    Services.audit.logdbWithLink(what, link, details:_*)
}


