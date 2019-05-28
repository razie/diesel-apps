package razie.diesel.model

import org.scalatest.selenium.WebBrowser.TagNameQuery
import razie.diesel.dom.WikiDomain
import razie.tconf.{SpecPath, TSpecPath}
import razie.wiki.model.{TagQuery, WikiSearch}

/** a message string - send these to Services to have them executed
  *
  * these will be executed as a new process/engine instance
  *
  * @param msg properly formatted message string, i.e. $msg ent.ac (p1="value")
  */
case class DieselMsgString(msg: String,
                           target: DieselTarget = DieselTarget.RK,
                           ctxParms: Map[String, String] = Map.empty) {
  def mkMsgString: String = {
    if (ctxParms.nonEmpty) {
      // add the params to the context with an artificial ctx.set message
      val extra = ctxParms.map(t => s"""${t._1} = "${t._2}"""").mkString(", ")
      s"""$$msg ctx.set($extra)\n\n$msg"""
    } else
      msg
  }

  def withContext(p: Map[String, String]) = this.copy(ctxParms = ctxParms ++ p)
}

/** schedule a message for later - send this to Services */
case class ScheduledDieselMsg(schedule: String, msg: DieselMsg) {}

/** a target for a message: either a specified list of config, or a realm
  *
  * @param specs list of specifications to get the rules from
  * @param stories optional list of stories to execute and validate
  */
class DieselTarget (val realm:String) {
  def specs: List[TSpecPath] = Nil
  def stories: List[TSpecPath] = Nil
}

/** a message intended for a target. Send to CQRS for execution, via Services */
case class DieselMsg(e: String,
                     a: String,
                     parms: Map[String, Any],
                     target: DieselTarget = DieselTarget.RK) {
  def toMsgString = DieselMsgString(
    s"$$msg $e.$a (" +
      (parms
        .map(
          t =>
            t._1 + "=" + (t._2 match {
              case s: String => s""" "$s" """
              case s: Int    => s"$s"
              case s @ _     => s"${s.toString}"
            })
        )
        .mkString(", ")) +
      ")",
    target
  )
}

object DieselTarget {

  def ENV_SETTINGS(realm:String) = SpecPath("", realm + ".Spec:EnvironmentSettings", realm)

  /** the environment settings - most common target */
  def from (realm:String, specs:List[TSpecPath], stories:List[TSpecPath]) =
    new DieselTargetList(realm, specs, stories)

  /** the environment settings - most common target */
  def ENV (realm:String) =
    new DieselTarget(realm) {
      override def specs = List(ENV_SETTINGS(realm))
    }

  /** the environment settings - most common target */
  def TQSPECS (realm:String, tq:TagQuery) =
    new DieselTarget(realm) {

      override def specs = {
        val irdom = WikiSearch.getList(realm, "", "", tq.and("spec").tags)

        ENV_SETTINGS(realm) :: irdom.map(_.wid.toSpecPath)
      }

      override def stories = {
        val irdom = WikiSearch.getList(realm, "", "", tq.and("story").tags)

        ENV_SETTINGS(realm) :: irdom.map(_.wid.toSpecPath)
      }
    }

  /** the environment settings - most common target */
  def RK =
    new DieselTarget("rk")

  /** all the specs */
//  def SPECS (realm:String) =
//    new DieselTarget(realm) {
//      override def specs = TagQuery("spec")
//    }

}

case class DieselTargetList(override val realm:String, override val specs:List[TSpecPath], override val stories:List[TSpecPath]) extends DieselTarget(realm)

object DieselMsg {
  final val REALM_LOADED = "$msg diesel.realm.loaded"

  final val GUARDIAN_POLL = "$msg diesel.guardian.poll"

  final val WIKI_UPDATED = "$msg rk.wiki.updated"

  final val GPOLL = "diesel.guardian.poll"
  final val RLOADED = "diesel.realm.loaded"

  final val irunDom = "irunDom:"
  final val runDom = "runDom:"

  object REALM {
    final val ENTITY = "diesel.realm"
    final val LOADED = "loaded"
  }
  object GUARDIAN {
    final val ENTITY = "diesel.guardian"

    final val NOTIFY = "notify"
    final val POLL = "poll"
    final val RUN = "run"
  }

  final val fiddleStoryUpdated = "fiddleStoryUpdated"
  final val fiddleSpecUpdated = "fiddleSpecUpdated"
}
