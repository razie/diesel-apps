/*  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

import org.bson.types.ObjectId

/** various settings for each wf engine, see http://specs.dieselapps.com/Topic/Flags_and_modes */
case class DomEngineSettings
(
  var mockMode    : Boolean = false,
  var blenderMode : Boolean = true,
  var draftMode   : Boolean = true,
  var sketchMode  : Boolean = false,
  var execMode    : String = "sync",
  var resultMode    : String = "",

  // when ran for a separate request
  var parentNodeId: Option[String] = None,

  /** tag for configuration: either a userId or a recognized global tag */
  var configTag : Option[String] = None,

  /** user id */
  var userId : Option[String] = None,

  /** content that was posted with the request */
  var postedContent : Option[EContent] = None,

  /** tag query to select for modeBlender */
  var tagQuery : Option[String] = None,

  var hostport : Option[String] = None,

  var realm : Option[String] = None,

  /** env target for this request, or None */
  var env : Option[String] = None,

  /** collector settings - how many of this kind to collect */
  var collectCount : Option[Int] = None,

  /** SLA settings - how to store and manage this instance. Default None means no persistance */
  var sla : Option[String] = None,

  var simMode : Boolean = false
) {
  val node = DieselAppContext.localNode // todo shouldn't I remember the node? or is that the hostport?

  // todo keep in sync with sla
  var slaSet =
    sla.map(_.split(","))
       .getOrElse(Array(DieselSLASettings.NOPERSIST, DieselSLASettings.KEEP1))

  /** is this supposed to use a user cfg */
  def configUserId = {
    configTag.map(x=>
      if(ObjectId.isValid(x)) Some(new ObjectId(x))
      else None
    )
  }

  def toJson : Map[String,String] = {
    import razie.diesel.engine.DomEngineSettings._
    Map(
      MOCK_MODE -> mockMode.toString,
      SKETCH_MODE -> sketchMode.toString,
      BLENDER_MODE -> blenderMode.toString,
      DRAFT_MODE -> draftMode.toString,
      EXEC_MODE -> execMode,
      RESULT_MODE -> resultMode
    ) ++ parentNodeId.map(x=> Map(DIESEL_NODE_ID -> x)
    ).getOrElse(Map.empty) ++ configTag.map(x=> Map(DIESEL_CONFIG_TAG -> x)
    ).getOrElse(Map.empty) ++ userId.map(x=> Map(DIESEL_USER_ID -> x)
    ).getOrElse(Map.empty) ++ tagQuery.map(x=> Map(TAG_QUERY -> x)
    ).getOrElse(Map.empty) ++ realm.map(x=> Map(REALM -> x)
    ).getOrElse(Map.empty) ++ env.map(x=> Map(ENV -> x)
    ).getOrElse(Map.empty) ++ collectCount.map(x=> Map(COLLECT -> x.toString)
    ).getOrElse(Map.empty) ++ sla.map(x=> Map(SLA -> x)
    ).getOrElse(Map.empty) ++ hostport.map(x=>
      Map(HOSTPORT -> x)
    ).getOrElse(Map.empty) ++
        Map(SIM_MODE -> simMode.toString)
  }

  override def toString = razie.js.tojsons(toJson)
}
object DomEngineSettings {
  final val SKETCH_MODE="sketchMode"
  final val MOCK_MODE="mockMode"
  final val BLENDER_MODE="blenderMode"
  final val DRAFT_MODE="draftMode"
  final val EXEC_MODE="execMode"
  final val RESULT_MODE="resultMode"
  final val DIESEL_NODE_ID = "dieselNodeId"
  final val DIESEL_CONFIG_TAG = "dieselConfigTag"
  final val DIESEL_USER_ID = "dieselUserId"
  final val TAG_QUERY = "tagQuery"
  final val HOSTPORT = "hostport"
  final val SIM_MODE = "simMode"
  final val REALM = "realm"
  final val ENV = "env"
  final val COLLECT = "dieselCollect"
  final val SLA = "dieselSLA"

  final val DFIDDLE = "dfiddle"
  final val INCLUDE_FOR = "includeFor"

  // filter qeury parms
  final val FILTER = Array(
    SKETCH_MODE, MOCK_MODE, BLENDER_MODE, DRAFT_MODE, EXEC_MODE,
    RESULT_MODE, SIM_MODE, DFIDDLE, INCLUDE_FOR, SLA, "saveMode"
  )

  /** take the settings from either URL or body form or default */
  def fromJson(j:Map[String, String]) = {
    def fqhParm(name:String) =
      j.get(name)

    def fqhoParm(name:String, dflt:String) =
      j.getOrElse(name, dflt)

    new DomEngineSettings(
      mockMode = fqhoParm(MOCK_MODE, "true").toBoolean,
      blenderMode = fqhoParm(BLENDER_MODE, "true").toBoolean,
      draftMode = fqhoParm(DRAFT_MODE, "true").toBoolean,
      sketchMode = fqhoParm(SKETCH_MODE, "false").toBoolean,
      execMode = fqhoParm(EXEC_MODE, "sync"),
      resultMode = fqhoParm(RESULT_MODE, ""),
      parentNodeId = fqhParm(DIESEL_NODE_ID),
      configTag = fqhParm(DIESEL_CONFIG_TAG),
      userId = fqhParm(DIESEL_USER_ID),
      hostport = fqhParm(HOSTPORT),
      realm = fqhParm(REALM),
      tagQuery = fqhParm(TAG_QUERY),
      env = fqhParm(ENV),
      collectCount = fqhParm(COLLECT).map(_.toInt),
      sla = fqhParm(SLA),
      simMode = fqhoParm(SIM_MODE, "true").toBoolean
    )
  }

}

object DieselSLASettings {
  // keep settings: how long to keep an instance
  final val NOKEEP = "nokeep" // don't keep at all
  final val KEEP1 = "keep1"   // keep short, in-mem, this is default
  final val KEEP2 = "keep2"   // keep med
  final val KEEP3 = "keep3"   // keep long

  final val NOPERSIST = "nopers" // no persistance, this is default
  final val PERSIST1 = "pers1" // persist fair
  final val PERSIST2 = "pers2" // persist good
  final val PERSIST3 = "pers3" // persist safe

  final val DEFAULT = "keep1,nopers"
}

