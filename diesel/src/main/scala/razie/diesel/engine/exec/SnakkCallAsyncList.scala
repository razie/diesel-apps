/*  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine.exec

import org.bson.types.ObjectId
import razie.{Logging, SnakkResponse}
import scala.collection.mutable
import scala.concurrent.Future

/** picked up and executed elsewhere - works with the SnakkCallServer */
object SnakkCallAsyncList extends Logging {
  case class Rec (id:String, sc:SnakkCall, time:Long = System.currentTimeMillis())

  var calls = new mutable.ListBuffer[Rec]()
  val inProgress = new mutable.HashMap[String, Rec]()

  val EXPIRY = 20000 // clean in 12 sec

  private def clean : Unit = {
    val now = System.currentTimeMillis()
    calls = calls.filter(now - _.time < EXPIRY)
    inProgress.find(now - _._2.time > EXPIRY).toList.foreach(t=>inProgress.remove(t._1))
  }

  def put (sc:SnakkCall) : Future[SnakkResponse] = {
    val rec = Rec(new ObjectId().toString, sc)
    log("ASCL put " + rec.id + " - " + sc.toJson)
    calls.synchronized {
      clean
      calls.append(rec)
    }
    sc.pro.get.future
  }

  /** next req waiting for host */
  def next(host:String) : Option[(String, SnakkCall)] = {
    calls.synchronized {
      clean
      val idx = calls.indexWhere(host == "*" || _.sc.url.startsWith(host))
      if(calls.size > 0 && idx >= 0) {
        val rec = calls.remove(idx)
        log("ASCL next " + rec.id + " - " + rec.sc.toJson)
        inProgress.synchronized {
          inProgress.put(rec.id, rec)
        }
        Some((rec.id, rec.sc))
      } else
        None
    }
  }

  def complete (id:String, resp:SnakkResponse) : Boolean = {
    log("Completing " + id + " with " + resp)
      inProgress.synchronized {
        val rec = inProgress.remove(id)
        rec.map {rec=>
          info("  Completing - found" + id)
          rec.sc.pro.foreach(_.success(resp))
          clean
          true
      } getOrElse {
          info("  Completing - NOT found" + id)
          clean
          false
        }
      }
  }
}

