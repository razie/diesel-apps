/*   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.expr

import razie.diesel.dom.RDOM.P
import razie.diesel.engine.DomAst
import razie.diesel.engine.nodes.EWarning

/** static context will delegate updates to parent - good as temporary override when evaluating a message */
class StaticECtx(cur: List[P] = Nil, base: Option[ECtx] = None, curNode:Option[DomAst]=None) extends SimpleECtx(cur, base, curNode) {

  // check for overwriting values
  // todo if i'm in a message, this will always be the case - why am I alarming?
  // todo and who can see it? it can't be in the logs...
  def check (p:P):Option[EWarning] = {
    if(cur.exists(_.name == p.name)) {
      Some(EWarning("WARNING_OVERWRITE at put - you may be overwriting ide-effects: "+p.name))
    } else None
  }

  //todo should I throw up if no base?
  override def put(p: P): Unit = {
    check(p)
    // propagate to base, so it lives
    base.map(_.put(p))
  }

  override def putAll(p: List[P]): Unit = {
    p.map(check)
    // propagate to base, so it lives
    base.map(_.putAll(p))
  }

  override def remove (name: String): Option[P] = {
    base.flatMap(_.remove(name))
  }

  override def clear = {
    base.map(_.clear)
  }

  override def toString = this.getClass.getSimpleName + ":" + cur.mkString //+ "\n base: " +base.toString
}

/** static context will delegate updates to parent - good as temporary override when evaluating a message */
class StaticECtxOverride(cur: List[P] = Nil, base: Option[ECtx] = None, curNode:Option[DomAst]=None) extends SimpleECtx(cur, base, curNode) {

  // copy overriding values from base - this is important when values are overwritten and the ctx is re-created
  attrs = base.toList.flatMap(_.asInstanceOf[SimpleECtx].attrs.filter{p=> cur.exists{c=> c.name == p.name}}) // attrs must have been calculated...

  // check for overwriting values
  // todo if i'm in a message, this will always be the case - why am I alarming?
  // todo and who can see it? it can't be in the logs...
  private def check (p:P) = {
//    if(cur.exists(_.name == p.name)) {
//      razie.Log.warn("WARNING_OVERWRITE at put - you may be overwriting side-effects: "+p.name)
//    }
  }

  //todo should I throw up if no base?
  override def put(p: P): Unit = {
    check(p)
    // propagate to base, so it lives
    base.map(_.put(p))
    // also overwrite here...
    attrs = p :: attrs.filter(_.name != p.name)
  }

  override def putAll(p: List[P]): Unit = {
    p.map(check)
    // propagate to base, so it lives
    base.map(_.putAll(p))
    // also overwrite here...
    attrs = p ::: attrs.filter(x => !p.exists(_.name == x.name))
  }

  override def remove (name: String): Option[P] = {
    attrs = attrs.filter(x => name == x.name)
    base.flatMap(_.remove(name))
  }

  override def clear = {
    base.map(_.clear)
  }

  override def toString = this.getClass.getSimpleName + ":" + cur.mkString //+ "\n base: " +base.toString
}

