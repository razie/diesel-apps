/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.model

import com.mongodb.casbah.Imports._
import org.joda.time.DateTime
import razie.db.{RCreate, RDelete, RMany, RTable}
import razie.db.tx.txn

/** staged stuff
  *
  * todo automatic purge of staged stuff that is forgotten
  */
@RTable
case class Stage (
  what: String,
  content: DBObject,
  by: String,   // userName
  crDtm: DateTime = DateTime.now,
  _id: ObjectId = new ObjectId()) {

  def create = RCreate.noAudit[Stage] (this) // no audit = not relevant
  def delete = RDelete[Stage]("_id"->_id)
}

object Staged {
  def find(what: String) = RMany[Stage]("what" -> what)
}
