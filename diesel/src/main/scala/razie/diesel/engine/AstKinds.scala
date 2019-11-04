/*   ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.engine

/** the kinds of nodes we understand */
object AstKinds {
  final val ROOT = "root"
  final val STORY = "story"
  final val RECEIVED = "received"
  final val SAMPLED = "sampled"

  final val ERROR = "error"
  final val DEBUG = "debug"
  final val TRACE = "trace"

  // these are like "info"
  final val GENERATED = "generated" // like info
  final val TEST = "test"
  final val BUILTIN = "built-in"

  final val SUBTRACE = "subtrace"
  final val RULE = "rule"
  final val SKETCHED = "sketched"
  final val MOCKED = "mocked"
  final val NEXT = "next"

  def isGenerated  (k:String) = GENERATED==k || SKETCHED==k || MOCKED==k || DEBUG==k || ERROR==k || TRACE==k
  def shouldIgnore (k:String) = RULE==k || BUILTIN==k
  def shouldSkip (k:String) = NEXT==k
  def shouldRollup (k:String) = NEXT==k
  def shouldPrune (k:String) = false

  /** kind based on arch so you can specify <trace> and all kids become "trace" */
  def kindOf (arch:String) = {
    val kind = arch match {
      case _ if arch contains "trace" => AstKinds.TRACE
      case _ if arch contains "debug" => AstKinds.DEBUG
      case _ => AstKinds.GENERATED
    }

    kind
  }
}
