/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.model

import org.bson.types.ObjectId
import razie.db.{RTable, RCreate, ROne, RUpdate}

/** keep track of view counts, per wiki page id */
@RTable
case class WikiCount (
  pid: ObjectId,
  count: Long = 1
//todo add thumbup, thumbdown
  ) {
  def inc = {
    WikiCount.findOne (pid) map (p=>
      RUpdate noAudit (Map("pid" -> pid), p.copy(count=p.count+1))
    ) orElse {
      RCreate noAudit this
      None
    }
  }
}

/** wiki factory and utils */
object WikiCount {
  def findOne(pid: ObjectId) = ROne[WikiCount] ("pid" -> pid)
}
