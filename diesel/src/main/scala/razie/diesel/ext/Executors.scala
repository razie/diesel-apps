/**
 *  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.ext

import razie.diesel.dom._

import scala.collection.mutable.ListBuffer

/** an applicable - can execute a message */
trait EApplicable {
  /** is this applicable... applicable? */
  def test(m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) : Boolean

  /** is this async?
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
    */
  def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any]
}

// can execute messages -
// todo can these add more decomosition or just proces leafs?
abstract class EExecutor (val name:String) extends EApplicable {
  def messages : List[EMsg] = Nil
}

object Executors {
  val _all = new ListBuffer[EExecutor]()

  def all : List[EExecutor] = _all.toList

  def add (e:EExecutor) = {_all append e}
}
