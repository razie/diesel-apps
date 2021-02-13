/**
  *    ____    __    ____  ____  ____,,___     ____  __  __  ____
  *   (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \          Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.model

import razie.audit.Audit
import razie.diesel.dom.RDOM.P.asString
import razie.diesel.engine.{DomEngine, DomEngineSettings}
import razie.diesel.engine.nodes.EMsg
import razie.diesel.samples.DomEngineUtils
import razie.diesel.samples.DomEngineUtils.extractResult
import razie.tconf.{SpecRef, TSpecRef, TagQuery}
import razie.wiki.model.{WID, WikiSearch, Wikis}
import razie.{clog, cout}

/** a message string - send these to Services to have them executed
  *
  * these will be executed as a new process/engine instance
  *
  * @param msg properly formatted message string, i.e. $msg ent.ac (p1="value")
  */
case class DieselMsgString(msg: String,
                           target: DieselTarget = DieselTarget.RK,
                           ctxParms: Map[String, String] = Map.empty,
                           osettings: Option[DomEngineSettings] = None,
                           omsg: Option[DieselMsg] = None
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

  def mkEngine = {
    val m = this
    // todo auth/auth
    cout << "======== DIESEL MSG: " + m
    val settings = osettings.getOrElse(new DomEngineSettings())
    settings.realm = Some(target.realm)
    // important to use None here, to let the engines use the envList setting otherwise
    settings.env = if (target.env == DieselTarget.DEFAULT) None else Some(target.env)

    val ms = m.mkMsgString

    import scala.concurrent.ExecutionContext.Implicits.global
    DomEngineUtils
        .createEngine(ms, target.specs, target.stories, settings, Some(this))
  }

  def postEngine(res: Map[String, Any]) = {
    // don't audit these frequent ones
    if (
      this.msg.startsWith(DieselMsg.WIKI_UPDATED) ||
          this.msg.startsWith(DieselMsg.CRON_TICK) ||
          this.msg.startsWith(DieselMsg.GUARDIAN_POLL) ||
          false//        m.msg.startsWith(DieselMsg.REALM_LOADED)
    ) {
      clog << "DIESEL_MSG: " + this + " : RESULT: " + res.get("value").mkString.take(500)
    } else {
      val id = res.get("engineId").mkString
      // this will also clog it - no need to clog it
      Audit.logdb(
        "DIESEL_MSG",
        this.toString,
        s"[[DieselEngine:$id]]",
        "result-length: " + res.get("value").mkString.length,
        res.get("value").mkString.take(500)
      )
    }
    res
  }

  def startMsg = {
    import scala.concurrent.ExecutionContext.Implicits.global
    mkEngine.process.map { engine =>
      extractResult(msg, None, engine)
    }.map(postEngine)
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

  def specs: List[TSpecRef] = Nil

  def stories: List[TSpecRef] = Nil
}

/** a message intended for a target. Send to CQRS for execution, via Services */
case class DieselMsg(
  e: String,
  a: String,
  args: Map[String, Any],
  target: DieselTarget = DieselTarget.RK,
  osettings:Option[DomEngineSettings] = None
) {

  def this(m: EMsg, target: DieselTarget) = this(
    m.entity,
    m.met,
    m.attrs.map(t => (t.name, t.currentStringValue)).toMap,
    target
  )

  def ea = e + "." + a

  def toMsgString = DieselMsgString(
    s"$$msg $e.$a (" +
        (args
            .map(
              t =>
                t._1 + "=" + (t._2 match {
                  case s: String if s.startsWith("{") || s.startsWith("[") => {
//                    val ss = s.replaceAll("\\\"", "\\\"")
                    s""" $s """
                  }
                  case s: String => {
                    // todo should escape them...
//                    val ss = s.replaceAll("\\\"", "\\\"")
                    s""" "$s" """
                  }
                  case s: Int => s"$s"
                  case s@_ => s"${s.toString}"
                })
            )
            .mkString(", ")) +
        ")",
    target,
    Map.empty,
    osettings,
    Some(this)
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

  /** find all specs starting from realm and tq, including mixins except private */
  def tqSpecs(realm: String, tq: TagQuery) = {
    // this and mixins
    val w = Wikis(realm)
    val tags = tq.and("spec").tags
    val irdom = (
        WikiSearch.getList(realm, "", "", tags) :::
            w.mixins.flattened.flatMap(r =>
              WikiSearch
                  .getList(r.realm, "", "", tags)
                  .filter(x => !(x.tags.contains("private")))
            )
        )

    irdom.map(_.wid.toSpecPath)
  }

  /** the environment settings - most common target */
  def ENV_SETTINGS(realm: String) =
    SpecRef(realm, realm + ".Spec:EnvironmentSettings", "EnvironmentSettings")

  /** specific list of specs to use */
  def from(realm: String, env: String, specs: List[TSpecRef], stories: List[TSpecRef]) =
    new DieselTargetList(realm, env, specs, stories)

  /** all specs in a realm and mixins */
  def ENV(realm: String, env: String = DEFAULT) =
    new DieselTarget(realm, env) {
      override def specs = {
        ENV_SETTINGS(realm) :: tqSpecs(realm, new TagQuery(""))
      }
    }

  /** the diesel section from the reactor topic */
  def REALMDIESEL(realm: String, env: String = DEFAULT) =
    DieselTarget.from(
      realm,
      env,
      WID.fromPath(s"${realm}.Reactor:${realm}#diesel").map(_.toSpecPath).toList,
      Nil)

  /** list of topics by tq */
  def TQSPECS(realm: String, env: String, tq: TagQuery) =
    new DieselTarget(realm, env) {

      override def specs = {
        ENV_SETTINGS(realm) :: tqSpecs(realm, tq)
      }
    }

  /** list of topics by tq */
  def TQSPECSANDSTORIES(realm: String, env: String, tq: TagQuery) =
    new DieselTarget(realm, env) {

      override def specs = {
        ENV_SETTINGS(realm) :: tqSpecs(realm, tq)
      }

      override def stories = {
        // stories just in current realm
        val irdom = WikiSearch.getList(realm, "", "", tq.and("story").tags)

        ENV_SETTINGS(realm) :: irdom.map(_.wid.toSpecPath)
      }
    }

  /** the environment settings - most common target */
  def RK = new DieselTarget("rk")
}

case class DieselTargetList(
  override val realm: String,
  override val env: String,
  override val specs: List[TSpecRef],
  override val stories: List[TSpecRef]) extends DieselTarget(realm)

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
    final val DIESEL_PUSH = "diesel.scope.push"
    final val DIESEL_POP = "diesel.scope.pop"
    final val RULE_PUSH = "diesel.scope.rule.push"
    final val RULE_POP = "diesel.scope.rule.pop"
  }

  object STREAMS {
    final val CREATE = "new"
    final val STREAM_PUT = "diesel.stream.put"
    final val STREAM_ONDATA = "diesel.stream.onData"
    final val STREAM_ONDATASLICE = "diesel.stream.onDataSlice"
    final val STREAM_ONDONE = "diesel.stream.onDone"
    final val STREAM_ONERROR = "diesel.stream.onError"
  }

  object ENGINE {
    final val ENTITY = "diesel"
    final val VALS = "vals"
    final val DIESEL_EXIT = "diesel.pleaseexit"
    final val DIESEL_CRASHAKKA = "diesel.crashakka"
    final val DIESEL_VALS = "diesel.vals"
    final val DIESEL_WARNINGS = "diesel.warnings"
    final val DIESEL_BEFORE = "diesel.before"
    final val DIESEL_AFTER = "diesel.after"
    final val DIESEL_LEVELS = "diesel.levels"
    final val DIESEL_RETURN = "diesel.return"
    final val DIESEL_THROW = "diesel.throw"
    final val DIESEL_PING = "diesel.ping"
    final val DIESEL_TRY = "diesel.try"
    final val DIESEL_CATCH = "diesel.catch"
    final val DIESEL_DEBUG = "diesel.debug"
    final val DIESEL_LATER = "diesel.later"
    final val DIESEL_WHILE = "diesel.while"
    final val DIESEL_ASSERT = "diesel.assert"
    final val DIESEL_MAP = "diesel.map"
    final val DIESEL_REST = "diesel.rest"
    final val DIESEL_SYNC = "diesel.engine.sync"
    final val DIESEL_ASYNC = "diesel.engine.async"
    final val DIESEL_PONG = "diesel.engine.pong"
    final val DIESEL_STEP = "diesel.step"
    final val DIESEL_TODO = "diesel.todo"

    final val STEP = "step"
    final val TODO = "todo"
    final val BEFORE = "before"
    final val AFTER = "after"
    final val DEBUG = "debug"

    final val DIESEL_MSG_ENTITY = "diesel.msg.entity"
    final val DIESEL_MSG_ACTION = "diesel.msg.action"
    final val DIESEL_MSG_EA = "diesel.msg.ea"
    final val DIESEL_MSG_ATTRS = "diesel.msg.attrs"

    final val DIESEL_ENG_SET = "diesel.engine.set"

    final val DIESEL_ENG_SETTINGS = "diesel.engine.settings"
    final val DIESEL_ENG_DESC = "diesel.engine.description"

    final val ERR_NORULESMATCH = "ERR_DIESEL_NORULESMATCH"
  }

  object HTTP {
    final val PREFIX        = "diesel.http."
    final val RESPONSE      = "diesel.http.response"
    final val STATUS        = "diesel.http.response.status"
    final val CTYPE         = "diesel.http.response.contentType"
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

  object APIGW {
    final val ENTITY = "diesel.apigw.limit"
  }

  object PROPS {
    final val ENTITY = "diesel.props"

    final val SET = "system"
    final val LIST = "file"
  }

  object IO {
    final val ENTITY = "diesel.io"

    final val TEXT_FILE = "diesel.io.textFile"
    final val LIST_FILES = "diesel.io.listFiles"
  }


  final val fiddleStoryUpdated = "fiddleStoryUpdated"
  final val fiddleSpecUpdated = "fiddleSpecUpdated"
}
