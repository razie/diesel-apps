/**
  *    ____    __    ____  ____  ____,,___     ____  __  __  ____
  *   (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \          Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.model

import razie.audit.Audit
import razie.{clog, cout}
import razie.diesel.engine.{DieselAppContext, DomEngineSettings}
import razie.diesel.samples.DomEngineUtils
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
                           ctxParms: Map[String, String] = Map.empty,
                           osettings:Option[DomEngineSettings] = None
                          ) {
  def mkMsgString: String = {
    if (ctxParms.nonEmpty) {
      // add the params to the context with an artificial ctx.set message
      val extra = ctxParms.map(t => s"""${t._1} = "${t._2}"""").mkString(", ")
      s"""$$msg ctx.set($extra)\n\n$msg"""
    } else
      msg
  }

  def withContext(p: Map[String, String]) = this.copy(ctxParms = ctxParms ++ p)

  def startMsg = {
    val m=this
    // todo auth/auth
    cout << "======== DIESEL MSG: " + m
    val settings = osettings.getOrElse(new DomEngineSettings())
    settings.realm = Some(target.realm)
    // important to use None here, to let the engines use the envList setting otherwise
    settings.env = if(target.env == DieselTarget.DEFAULT) None else Some(target.env)

    val ms = m.mkMsgString

    import scala.concurrent.ExecutionContext.Implicits.global
    DomEngineUtils
        .runDom(ms, target.specs, target.stories, settings)
        .map { res =>
          // don't audit these frequent ones
          if(
            m.msg.startsWith(DieselMsg.WIKI_UPDATED) ||
                m.msg.startsWith(DieselMsg.CRON_TICK) ||
                m.msg.startsWith(DieselMsg.GUARDIAN_POLL) ||
                false//        m.msg.startsWith(DieselMsg.REALM_LOADED)
          ) {
            clog << "DIESEL_MSG: " + m + " : RESULT: " + res.get("value").mkString.take(500)
          } else {
            val id = res.get("engineId").mkString
            // this will also clog it - no need to clog it
            Audit.logdb(
              "DIESEL_MSG",
              m.toString,
              s"[[DieselEngine:$id]]",
              "result-length: "+res.get("value").mkString.length,
              res.get("value").mkString.take(500)
            )
          }
        }
  }
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
    target,
    Map.empty,
    osettings
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
      override def specs = {
        val tq = new TagQuery("")
        val irdom = WikiSearch.getList(realm, "", "", tq.and("spec").tags)

        ENV_SETTINGS(realm) :: irdom.map(_.wid.toSpecPath)
      }

      // all specs, not just envset
//      override def specs = List(ENV_SETTINGS(realm))
    }

  /** the diesel section from the reactor topic */
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
  final val GUARDIAN_ENDS = "$msg diesel.guardian.ends"
  final val WIKI_UPDATED = "$msg diesel.wiki.updated"
  final val USER_JOINED = "$msg diesel.user.joined"

  final val GPOLL = "diesel.guardian.poll"

  final val irunDom = "irunDom:"
  final val runDom = "runDom:"

  object REALM {
    final val REALM_LOADED_MSG = "$msg diesel.realm.loaded"
    final val REALM_LOADED = "diesel.realm.loaded"
    final val REALM_CONFIGURE = "diesel.realm.configure"
    final val REALM_SET = "diesel.realm.set"
    final val ENTITY = "diesel.realm"
    final val LOADED = "loaded"
    final val CONFIGURE = "configure"
  }

  object SCOPE {
    final val ENTITY = "diesel.scope"
    final val DIESEL_PUSH ="diesel.scope.push"
    final val DIESEL_POP = "diesel.scope.pop"
  }

  object ENGINE {
    final val ENTITY = "diesel"
    final val VALS = "vals"
    final val DIESEL_RETURN = "diesel.return"
    final val DIESEL_THROW = "diesel.throw"
    final val DIESEL_CATCH = "diesel.catch"
    final val DIESEL_VALS = "diesel.vals"
    final val DIESEL_BEFORE = "diesel.before"
    final val DIESEL_AFTER = "diesel.after"
    final val DIESEL_DEBUG = "diesel.debug"
    final val DIESEL_LATER = "diesel.later"
    final val DIESEL_REST = "diesel.rest"
    final val BEFORE = "before"
    final val AFTER = "after"
    final val DEBUG = "debug"

    final val DIESEL_MSG_ENTITY = "diesel.msg.entity"
    final val DIESEL_MSG_ACTION = "diesel.msg.action"
    final val DIESEL_MSG_EA     = "diesel.msg.ea"
    final val DIESEL_MSG_ATTRS  = "diesel.msg.attrs"

    final val DIESEL_ENG_SET = "diesel.engine.set"

    final val DIESEL_ENG_SETTINGS = "diesel.engine.settings"
    final val DIESEL_ENG_DESC =     "diesel.engine.description"

    final val ERR_NORULESMATCH = "ERR_DIESEL_NORULESMATCH"
  }

  object HTTP {
    final val PREFIX        = "diesel.http."
    final val RESPONSE      = "diesel.http.response"
    final val STATUS        = "diesel.http.response.status"
    final val HEADER_PREFIX = "diesel.http.response.header."
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

  object PROPS {
    final val ENTITY = "diesel.props"

    final val SET = "system"
    final val LIST = "file"
  }

  final val fiddleStoryUpdated = "fiddleStoryUpdated"
  final val fiddleSpecUpdated = "fiddleSpecUpdated"
}
