/**
  *   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.utils

import org.joda.time.DateTime
import razie.diesel.engine.{DieselSLASettings, DomEngine, DomState}
import razie.diesel.model.DieselMsg

// todo make instance in Reactor instead of a static
/** collects the last engine traces */
object DomCollector {

  final val MAX_SIZE = 200 // how many to keep
  final val MAX_SIZE_LOWER = 50 // how many to keep

  case class CollectedAst(stream:String, realm:String, id:String, userId:Option[String], engine:DomEngine, details:String, dtm:DateTime=DateTime.now) {
    def isLowerPriority = {
      val desc = engine.description
      desc.contains(DieselMsg.fiddleStoryUpdated) ||
          desc.contains(DieselMsg.REALM.REALM_LOADED_MSG) ||
          desc.contains("$msg " + DieselMsg.GPOLL)
    }

    def getMaxCount = {
      engine.settings.collectCount.filter(_ != 0).getOrElse {
        val desc = engine.description
        if(
          desc.contains(DieselMsg.fiddleStoryUpdated) ||
          desc.contains(DieselMsg.GPOLL)
        ) 6 else {
          if(engine.settings.slaSet.contains(DieselSLASettings.NOKEEP)) -1 else 0
          // 0  means no self-imposed limit, default
        }
      }
    }
  }

// statically collecting the last 100 results sets
  private var asts: List[CollectedAst] = Nil

  def withAsts[T] (f: List[CollectedAst] =>T) : T = asts.synchronized {
    f(asts)
  }

  /** statically collect more asts */
  def collectAst (stream:String, realm:String, xid:String, userId:Option[String], eng:DomEngine, details:String="") = synchronized {
    var newOne = CollectedAst(stream, realm, xid, userId, eng, details)
    var newAsts = newOne :: asts.filter(_.id != xid)
    val count = newOne.getMaxCount

    if(count >= 0) { // no collect

      // does it have collect settings?
      if (count > 0) {
        var lesser = newAsts.filter(_.engine.description == eng.description)

        while(lesser.size > count) {
          // remove one of this kind
          lesser.reverse.find(e=>DomState.isDone(e.engine.status)).map(_.id).foreach{lastId=>
            newAsts = newAsts.filter(_.id != lastId).take(MAX_SIZE - 1)
            lesser = newAsts.filter(_.engine.description == eng.description)
          }
        }
      }

      // if too many lower prio now, remove one
      val lower = asts.filter(_.isLowerPriority)

      if (lower.size > MAX_SIZE_LOWER) {
        lower.reverse.find(e=>DomState.isDone(e.engine.status)).map(_.id).foreach { lastId =>
          // try to remove first a lower priority one
          asts = newAsts.filter(_.id != lastId).take(MAX_SIZE - 1)
        }
      } else {
        asts = newAsts.take(MAX_SIZE - 1)
      }
    }
  }

  /** statically collect more asts */
  def cleanAst = synchronized {
    asts = Nil
  }

}

