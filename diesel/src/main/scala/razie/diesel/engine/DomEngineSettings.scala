/**
 *  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

import org.bson.types.ObjectId
import razie.diesel.engine.RDExt.EEContent

object DomEngineSettings {
  /** take the settings from either URL or body form or default */
  def fromJson(j:Map[String, String]) = {
    def fqhParm(name:String) =
      j.get(name)

    def fqhoParm(name:String, dflt:String) =
      j.get(name).getOrElse(dflt)

    new DomEngineSettings(
      mockMode = fqhoParm(MOCK_MODE, "true").toBoolean,
      blenderMode = fqhoParm(BLENDER_MODE, "true").toBoolean,
      draftMode = fqhoParm(DRAFT_MODE, "true").toBoolean,
      sketchMode = fqhoParm(SKETCH_MODE, "false").toBoolean,
      execMode = fqhoParm(EXEC_MODE, "sync"),
      resultMode = fqhoParm(RESULT_MODE, "json"),
      parentNodeId = fqhParm(DIESEL_NODE_ID),
      configTag = fqhParm(DIESEL_CONFIG_TAG),
      userId = fqhParm(DIESEL_USER_ID),
      hostport = fqhParm(HOSTPORT),
      realm = fqhParm(REALM),
      tagQuery = fqhParm(TAG_QUERY)
    )
  }

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
  final val REALM = "realm"

  // filter qeury parms
  final val FILTER = Array(SKETCH_MODE, MOCK_MODE, BLENDER_MODE, DRAFT_MODE, EXEC_MODE, RESULT_MODE)
}

case class DomEngineSettings
(
  var mockMode    : Boolean = false,
  var blenderMode : Boolean = true,
  var draftMode   : Boolean = true,
  var sketchMode  : Boolean = true,
  var execMode    : String = "sync",
  var resultMode    : String = "json",

  // when ran for a separate request
  var parentNodeId: Option[String] = None,

  /** tag for configuration: either a userId or a recognized global tag */
  var configTag : Option[String] = None,

  /** user id */
  var userId : Option[String] = None,

  /** content that was posted with the request */
  var postedContent : Option[EEContent] = None,

  /** tag query to select for modeBlender */
  var tagQuery : Option[String] = None,

  var hostport : Option[String] = None,

  var realm : Option[String] = None
  ) {
  val node = DieselAppContext.localNode

  /** is this supposed to use a user cfg */
  def configUserId = configTag.map(x=>if(ObjectId.isValid(x)) Some(new ObjectId(x)) else None)

  def toJson : Map[String,String] = {
    import DomEngineSettings._
    Map(
      MOCK_MODE -> mockMode.toString,
      SKETCH_MODE -> sketchMode.toString,
      BLENDER_MODE -> blenderMode.toString,
      DRAFT_MODE -> draftMode.toString,
      EXEC_MODE -> execMode,
      RESULT_MODE -> resultMode
    ) ++ parentNodeId.map(x=>
      Map(DIESEL_NODE_ID -> x)
    ).getOrElse(Map.empty) ++ configTag.map(x=>
      Map(DIESEL_CONFIG_TAG -> x)
    ).getOrElse(Map.empty) ++ userId.map(x=>
      Map(DIESEL_USER_ID -> x)
    ).getOrElse(Map.empty) ++ tagQuery.map(x=>
      Map(TAG_QUERY -> x)
    ).getOrElse(Map.empty) ++ realm.map(x=>
      Map(REALM -> x)
    ).getOrElse(Map.empty) ++ hostport.map(x=>
      Map(HOSTPORT -> x)
    ).getOrElse(Map.empty)
  }
}


