/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.model.features

import org.bson.types.ObjectId
import razie.clog
import razie.db._
import razie.db.tx.txn
import play.api.cache._
import play.api.Play.current

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
    WikiCount.findOne (pid) map {p=>
      val newone = p.copy(count=p.count+1)
      RUpdate noAudit (Map("pid" -> pid), newone)
      Cache.set("count."+pid.toString, newone, 300) // 10 minutes
    } orElse {
      RCreate noAudit this
      Cache.set("count."+pid.toString, this, 300) // 10 minutes
      None
    }
  }

  def set (newCount:Long) = {
    //todo optimize use upsert
    WikiCount.findOne (pid) foreach {p=>
      val newone = p.copy(count=newCount)
      RUpdate (Map("pid" -> pid), newone)
      Cache.set("count."+pid.toString, newone, 300) // 10 minutes
    }
  }
}

/** wiki factory and utils */
object WikiCount {

  def findOne(pid: ObjectId) = {
    Cache.getAs[WikiCount]("count."+pid.toString).map { x =>
      Some(x)
    }.getOrElse {
      val x = ROne[WikiCount] ("pid" -> pid)
      x.map(x=>Cache.set("count."+x.pid.toString, x, 300)) // 10 minutes
      x
    }
  }
}
