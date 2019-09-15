package razie.diesel.model

import razie.diesel.engine.DomEngineSettings
import razie.tconf.{SpecPath, TSpecPath, TagQuery}
import razie.wiki.model.{WID, WikiSearch}

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
  * @param realm - the target realm
  * @param env - the target env inside the target realm
  * @param specs - list of specifications to get the rules from
  * @param stories - optional list of stories to execute and validate
  */
class DieselTarget (
  val realm:String,
  val env:String = DieselTarget.DEFAULT) {

  def specs: List[TSpecPath] = Nil
  def stories: List[TSpecPath] = Nil
}

/** a message intended for a target. Send to CQRS for execution, via Services */
case class DieselMsg(
  e: String,
  a: String,
  args: Map[String, Any],
  target: DieselTarget = DieselTarget.RK,
  osettings:Option[DomEngineSettings] = None
) {

  def toMsgString = DieselMsgString(
    s"$$msg $e.$a (" +
      (args
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

  def toJson : Map[String,Any] = {
    Map(
      "e" -> e,
      "a" -> a,
      "ea" -> (e+"."+a),
      "args" -> args,
      "target" -> target.toString
    ) ++ osettings.map(x=> Map("osettings" -> x.toJson)
    ).getOrElse(Map.empty)
  }

  override def toString = razie.js.tojsons(toJson)
}

object DieselTarget {
  final val DEFAULT = "default"

  def ENV_SETTINGS(realm:String) = SpecPath("", realm + ".Spec:EnvironmentSettings", realm)

  /** the environment settings - most common target */
  def from (realm:String, env:String, specs:List[TSpecPath], stories:List[TSpecPath]) =
    new DieselTargetList(realm, env, specs, stories)

  /** the environment settings - most common target */
  def ENV (realm:String, env:String=DEFAULT) =
    new DieselTarget(realm, env) {
      override def specs = List(ENV_SETTINGS(realm))
    }

  /** the environment settings - most common target */
  def REALMDIESEL (realm:String, env:String=DEFAULT) =
    DieselTarget.from(
      realm,
      env,
      WID.fromPath(s"${realm}.Reactor:${realm}#diesel").map(_.toSpecPath).toList,
      Nil)

  /** the environment settings - most common target */
  def TQSPECS (realm:String, env:String, tq:TagQuery) =
    new DieselTarget(realm, env) {

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

case class DieselTargetList(
  override val realm:String,
  override val env:String,
  override val specs:List[TSpecPath],
  override val stories:List[TSpecPath]) extends DieselTarget(realm)

object DieselMsg {
  final val CRON_TICK = "$msg diesel.cron.tick"
  final val GUARDIAN_POLL = "$msg diesel.guardian.poll"
  final val GUARDIAN_RUN = "$msg diesel.guardian.run"
  final val WIKI_UPDATED = "$msg diesel.wiki.updated"
  final val USER_JOINED = "$msg diesel.user.joined"

  final val GPOLL = "diesel.guardian.poll"

  final val irunDom = "irunDom:"
  final val runDom = "runDom:"

  object REALM {
    final val REALM_LOADED_MSG = "$msg diesel.realm.loaded"
    final val REALM_LOADED = "diesel.realm.loaded"
    final val ENTITY = "diesel.realm"
    final val LOADED = "loaded"
  }

  object SCOPE {
    final val ENTITY = "diesel.scope"
    final val PUSH = "push"
    final val POP = "pop"
  }

  object ENGINE {
    final val ENTITY = "diesel"
    final val VALS = "vals"
    final val BEFORE = "before"
    final val AFTER = "after"
    final val DEBUG = "debug"
  }

  object GUARDIAN {
    final val ENTITY = "diesel.guardian"

    final val STARTS = "starts"
    final val NOTIFY = "notify"
    final val POLL = "poll"
    final val RUN = "run"
  }

  object CRON {
    final val ENTITY = "diesel.cron"

    final val SET = "set"
    final val LIST = "list"
    final val TICK = "tick"
    final val STOP = "stop"
  }

  final val fiddleStoryUpdated = "fiddleStoryUpdated"
  final val fiddleSpecUpdated = "fiddleSpecUpdated"
}
