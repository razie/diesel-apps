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

  final val MAX_SIZE = 200 // how many to keep overall
  final val MAX_SIZE_LOWER = 50 // how many to keep, for lower priority traces

  /** a single collected trace */
  case class CollectedAst(stream: String, realm: String, id: String, userId: Option[String], engine: DomEngine,
                          collectGroup: String, details: String, dtm: DateTime = DateTime.now) {
    def isLowerPriority = {
      val desc = engine.description // not collectGroup
      desc.contains(DieselMsg.fiddleStoryUpdated) ||
          desc.contains(DieselMsg.REALM.REALM_LOADED_MSG) ||
          desc.endsWith(DieselMsg.ENGINE.DIESEL_PING) ||
          desc.contains("$msg " + DieselMsg.GPOLL)
    }

    /** how many of these to keep in trace collector? */
    def getMaxCount = {
      engine.settings.collectCount.filter(_ != 0).getOrElse {
        val desc = engine.collectGroup
        val res = if (
          desc.endsWith(DieselMsg.ENGINE.DIESEL_PING)
        ) 3 else if (
          desc.contains(DieselMsg.fiddleStoryUpdated) ||
              desc.contains(DieselMsg.GPOLL)
        ) 6 else {
          if (engine.settings.slaSet.contains(DieselSLASettings.NOKEEP)) -1 else 0
          // 0  means no self-imposed limit, default
        }
        res
      }
    }
  }

  /* statically collecting the last MAX_SIZE results sets **/
  private var asts: List[CollectedAst] = Nil

  /** encapsulate access to sync and collect resources */
  def withAsts[T](f: List[CollectedAst] => T): T = synchronized {
    f(asts)
  }

  /** statically collect more asts */
  def collectAst(
    stream: String, realm: String, xid: String,
    userId: Option[String], eng: DomEngine, details: String = "") = synchronized {

    val collectGroup = eng.collectGroup
    val newOne = CollectedAst(stream, realm, xid, userId, eng, collectGroup, details)
    val count = newOne.getMaxCount

    if (count >= 0) { // collect it

      var newAsts = newOne :: asts.filter(_.id != xid) // make sure no duplos

      // does it have collect settings? collect settings are per description
      if (count > 0) {
        //        var lesser = newAsts.filter(_.engine.description == eng.description)
        var lesser = newAsts.filter(_.collectGroup == collectGroup)

        // if it does and collected more in the same description, remove some
        var stupidLimit = MAX_SIZE // bug 636 - had a logic issue with 2.1.5.t6, too risky to rethink now, so put a
        // stupid limit in place
        while (lesser.size > count && stupidLimit > 0) {
          stupidLimit = stupidLimit - 1
          // remove one of this kind, done
          lesser.reverse.find(e => DomState.isDone(e.engine.status)).map(_.id).foreach { lastId =>
            newAsts = newAsts.filter(_.id != lastId).take(MAX_SIZE - 1)
            lesser = newAsts.filter(_.collectGroup == collectGroup)
          }

          // bug 636 - it was spinning if too many flows in progress with same description
          if (lesser.size > count) {
            // remove some of the in progress flows too
            lesser.lastOption.map(_.id).foreach { lastId =>
              newAsts = newAsts.filter(_.id != lastId).take(MAX_SIZE - 1)
              lesser = newAsts.filter(_.collectGroup == collectGroup)
            }
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

