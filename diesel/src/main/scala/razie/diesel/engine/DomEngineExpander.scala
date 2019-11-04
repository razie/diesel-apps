/*  ____    __    ____  ____  ____  ___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

import razie.diesel.engine.nodes.EMsg
import razie.diesel.expr._

/** engine execution strategy - to be implemented by specific engines
  *
  * the strategy takes care of the actual execution of nodes.
  *
  * the process is call "expansion" because the node execution can result in sub-activities. A
  * node execution/expansion can :
  * - create data in context
  * - create more child nodes, some for info purposes and some that may need executing themselves
  * - side effects outside
  */
trait DomEngineExpander {

  /** expand/execute a node */

  protected def expand(a: DomAst, recurse: Boolean = true, level: Int): List[DEMsg]
  /** expand/execute a message node */
  protected def expandEMsg(a: DomAst, in: EMsg, recurse: Boolean, level: Int, parentCtx:ECtx) : List[DomAst]

}

