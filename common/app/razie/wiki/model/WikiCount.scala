/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.model

import org.bson.types.ObjectId
import razie.db._
import razie.db.tx.txn

/** keep track of view counts, per wiki page id */
@RTable
case class WikiCount (
  pid: ObjectId,
  count: Long = 1,
  _id:ObjectId = new ObjectId()
//todo add thumbup, thumbdown
  ) extends REntity[WikiCount] {
  def inc = {
    //todo optimize use upsert
    WikiCount.findOne (pid) map (p=>
      RUpdate noAudit (Map("pid" -> pid), p.copy(count=p.count+1))
      ) orElse {
      RCreate noAudit this
      None
    }
  }
  def set (newCount:Long) = {
    //todo optimize use upsert
    WikiCount.findOne (pid) foreach (p=>
      RUpdate (Map("pid" -> pid), p.copy(count=newCount))
    )
  }
}

/** wiki factory and utils */
object WikiCount {
  def findOne(pid: ObjectId) = ROne[WikiCount] ("pid" -> pid)
}
