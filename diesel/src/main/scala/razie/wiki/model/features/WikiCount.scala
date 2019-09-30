/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.model.features

import org.bson.types.ObjectId
import play.api.Play.current
import play.api.cache._
import razie.db._
import razie.db.tx.txn

/** keep track of view counts, per wiki page id
  *
  * use pid - page ID only, for regular topics
  *
  * use wpath only for special pages (like hardcoded templates). make sure the wpaths is realm-neutral (i.e. make it up with category:name)
  */
@RTable
case class WikiCount (
  pid: ObjectId,
  count: Long = 1,
  wpath: Option[String] = None,
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

  def findOneForTemplate(wpath: String) = {
    // todo optimize with Cache ?
    val x = ROne[WikiCount] ("wpath" -> wpath)
    x.orElse(
      Some(
        WikiCount(new ObjectId(), 1, Some(wpath))
      )
    )
  }
}
