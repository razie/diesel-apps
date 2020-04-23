/*  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine.exec

import razie.diesel.engine.nodes.{EMsg, MatchCollector}
import razie.diesel.expr.ECtx
import scala.collection.concurrent.TrieMap

/** an applicable or message executor - can execute a message */
trait EApplicable {

  /** is this applicable to the given message?
    *
    * @param m the message to test applicability on
    * @param cole collect match/no-match info
    * @param ctx context
    * @return true/false if this executor can execute this message. it's up to the engine's strategy to use this info
    */
  def test(m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) : Boolean

  /** is this async? note that this flag is just for info, not needed, but do read this comment on async execs
    *
    * If SYNC, we'll wait in this thread on the call to apply - avoid a switch. This is great for
    * local support like DB, logging, echo and other services
    *
    * If ASYNC, then the engine will actor it out and you'll need to send a DERep to the engine when done
    * so it's more like asking the engine to isolate you rather than promising something
    *
    * In case this is async, you can return some info/correlation nodes from apply(), which will be added to the tree,
    * after which the engine will NOT mark this node complete. You will send a DERep when done.
    *
    * Also, while processing, you could add more info nodes to this one and only at the end mark it as done with DERep.
    *
    * todo will need a DEDetails for the temp info nodes
    * todo implement async, with examples
    */
  def isAsync : Boolean = false

  /** is this a mock? is it supposed to run in mock mode or not?
    *
    * you can have an executor for mock mode and one for normal mode
    */
  def isMock : Boolean = false

  /** do it !
    *
    * @return a list of elements - these will be wrapped in DomAst and added to the tree, so a value should be EVal etc
    *
    * When starting an async action yourself, just return an EEngSuspend and schedule a later DERep to complete this actibity
    */
  def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any]
}

/**
  * a message executor - these can decompose leafs into values or generate more messages
  *
  * Executors are hardcoded logic, connectors etc
  *
  * @param name - the name of this executor
  */
abstract class EExecutor (val name:String) extends EApplicable {
  /** the list of message specs for this executor - overwrite and return them for content assist. this is not neccessary
    * for the functioning of the executor - it's just for content assist
    *
    * it is preferable and easier to have them defined in one of the specs included in your flows
    */
  def messages : List[EMsg] = Nil
}

/** manage all executors */
object Executors {
  private var _all : TrieMap[String, EExecutor] = TrieMap()

  def withAll[T] (f: TrieMap[String, EExecutor] => T) : T = {
    f(_all)
  }

  def add (e:EExecutor) = {
    _all.put(e.name, e)
  }
}


