/**
  *   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 **/
package razie.audit

import com.mongodb.casbah.Imports._
import com.novus.salat.grater
import org.bson.types.ObjectId
import org.joda.time.DateTime
import razie.Logging
import razie.db.RazSalatContext.ctx
import razie.db.{ROne, RazMongo}
import razie.wiki.{BasicServices}

/**
  * diesel's default Audit implementation - stores them events in a Mongo table. Use this as an example to write your own auditing service.
  *
  * Upon review, move them to the cleared/history table and purge them sometimes
  */
class MdbAuditService extends AuditService with Logging {

  /** log an audit event */
  def logdb(what: String, details: Any*) = {
    val d = details.mkString(",")
    BasicServices ! Audit("a", what, d) // there will be an async processor here to take them
    val s = what + " " + d
    razie.Log.audit(s)
    s
  }

  /** log an audit event related to a link-able entity */
  def logdbWithLink(what: String, link: String, details: Any*) = {
    val d = details.mkString(",")
    BasicServices ! Audit("a", what, d, Some(link)) // there will be an async processor here to take them
    val s = what + " " + d
    razie.Log.audit(s)
    s
  }
}

object ClearAudits {

  /** move from review to archive. archive is purged separately. */
  def clearAudit(id: String, userId: String) = {
    ROne[Audit](new ObjectId(id)) map { ae =>
      val o = grater[Audit].asDBObject(ae)
      o.putAll(Map("clearedBy" -> userId, "clearedDtm" -> DateTime.now))
      RazMongo("AuditCleared") += o
      RazMongo("Audit").remove(Map("_id" -> new ObjectId(id)))
    }
  }
}
