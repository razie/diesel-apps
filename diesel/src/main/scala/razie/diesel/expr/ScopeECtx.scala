/*   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.expr

import razie.diesel.dom.RDOM.P
import razie.diesel.engine.DomAst

/** context for an internal scope - parent is scope or Eng
  *
  * todo when saving a context, do I save children too?
  *
  * todo when loading context, how do I reover active scope contexts
  */
class ScopeECtx(cur: List[P] = Nil, base: Option[ECtx] = None, curNode:Option[DomAst]=None) extends SimpleECtx(cur, base, curNode) {
  override def put(p: P): Unit =
    attrs = p :: attrs.filter(_.name != p.name)

  override def putAll(p: List[P]): Unit =
    attrs = p ::: attrs.filter(x => !p.exists(_.name == x.name))

  override def clear = {
    attrs = Nil
    // don't cascade to base
  }

  override def toString = this.getClass.getSimpleName + ":" + cur.mkString// + "\n base: " +base.toString
}


