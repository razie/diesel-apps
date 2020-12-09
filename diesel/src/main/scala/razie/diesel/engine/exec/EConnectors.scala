/*  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine.exec

import razie.base.AttrAccess
import razie.diesel.dom.RDOM.{P, PValue}
import razie.diesel.dom._
import razie.diesel.engine.DomAst
import razie.diesel.engine.nodes.{EMsg, MatchCollector}
import razie.diesel.expr.{ECtx, SimpleECtx}
import scala.collection.concurrent.TrieMap

/** these are assignable and take action when assigned */
trait EAssignable {
  // assignables have names?
  def name: String

  /** copy yourself with a different name. don't forget to remove and re-add to EEConnectors */
  def assigningTo(p: RDOM.P): PValue[EAssignable]
}

//======================================================

/**
  * todo WIP
  *
  * basic connectors - these maintain state and have a lifecycle, possibly actors etc
  *
  * @param name - the name of this executor
  */
abstract class EEConnector(val name: String, val ttype: String) extends EApplicable with EAssignable {
  /** the list of message specs for this executor - overwrite and return them for content assist */
  def messages: List[EMsg] = Nil

  override def test(ast: DomAst, m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx): Boolean = true

  // todo lifecycle - wire these up somewhere, the stop
  def connect = {}

  def reconnect = {}

  def stopNow = {}

  def stop = {}

  def status: String = EEConnectors.STATUS_INIT

  // todo add conn management parameters

  // each can specify a different manager, with a default. Managers manage lifecycle, reconnecting etc
  def getConnManager = ???

  def getConnManagerProps: AttrAccess = ???
}

/** manage all connectors */
object EEConnectors {
  private var _all: TrieMap[String, EEConnector] = TrieMap()

  /** in the map they are indexed by realm-name */
  def withAll[T](f: Map[String, EEConnector] => T): T = {
    f(_all.toMap)
  }

  def add(realm: String, e: EEConnector) = {
    RealmContexts.put(realm, P(e.name, "", WTypes.wt.OBJECT).withValue(e, WTypes.wt.OBJECT))
    _all.put(s"$realm-${e.name}", e)
  }

  def get(realm: String, name: String) = {
    _all.get(s"$realm-$name")
  }

  def remove(realm: String, name: String) = {
    RealmContexts.remove(realm, name)
    _all.remove(s"$realm-$name")
  }

  val STATUS_INIT = "init"
  val STATUS_OPEN = "open"
  val STATUS_CLOSED = "closed"
}

/** per-realm statics
  *
  * todo right now I accept and grow with any realm - when doing realm-node assignments, clean that up
  */
object RealmContexts {
  private var _all: TrieMap[String, ECtx] = TrieMap()

  /** in the map they are indexed by realm-name */
  def withAll[T](f: Map[String, ECtx] => T): T = {
    f(_all.toMap)
  }

  def put(realm: String, e: P) = {
    _all.getOrElseUpdate(realm, new SimpleECtx).put(e)
  }

  def get(realm: String, name: String) = {
    _all.getOrElseUpdate(realm, new SimpleECtx).getp(name)
  }

  def remove(realm: String, name: String) = {
    _all.getOrElseUpdate(realm, new SimpleECtx).remove(name)
  }
}

