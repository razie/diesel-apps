/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.model

import org.bson.types.ObjectId
import org.joda.time.DateTime
import razie.db.{RTable, RCreate, Txn, tx}

/**
 * auditing events on wiki pages, like edits, views etc
 *
 * TODO should probably allow customization to track only interesting events, i.e. filters
 *
 * for now - just purge the table as you feel like...
 */
@RTable
case class WikiAudit(
  event: String,
  wpath: String,
  userId: Option[ObjectId],
  details: Option[String] = None, // extra details
  crDtm: DateTime = DateTime.now,
  _id: ObjectId = new ObjectId()) {
  def create(implicit txn: Txn = tx.auto) = RCreate noAudit this
}
