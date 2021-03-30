/*   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.expr

import razie.diesel.Diesel
import razie.diesel.dom.RDOM.P
import razie.diesel.engine.DomAst
import razie.diesel.engine.nodes.EWarning

/** each rule has its own scope, like function calls. you can only set here and higher with `scopeVars.x=y`
  *
  * todo when saving a context, do I save children too?
  *
  * todo when loading context, how do I reover active scope contexts
  *
  * NOTE this is used to demarcate rule scopes too
  */
class RuleScopeECtx(
  cur: List[P] = Nil,
  base: Option[ECtx] = None,
  curNode: Option[DomAst] = None)
    extends LocalECtx(cur, base, curNode) {

  override def put(p: P): Unit = {
    // some parms propagate up - CANNOT duplicate here or updates to the upper version are lost
    if (Diesel.PAYLOAD == p.name) {
      // don't warn like LocalECtx
      base.foreach(_.put(p))
    } else {
      // overwrite here, don't propagate up
      attrs = p :: attrs.filter(_.name != p.name)
    }
  }
}


/** local context - sets don't propagate up */
class LocalECtx(
  cur: List[P] = Nil,
  base: Option[ECtx] = None,
  curNode: Option[DomAst] = None) extends SimpleECtx(cur, base, curNode) {

//   copy overriding values from base - this is important when values are overwritten and the ctx is re-created
//  attrs = base.toList.flatMap(_.asInstanceOf[SimpleECtx].attrs.filter { p =>
//    cur.exists { c => c.name == p.name }
//  }) // attrs must have been calculated...

  //todo should I throw up if no base?
  override def put(p: P): Unit = {
    // some parms propagate up - CANNOT duplicate here or updates to the upper version are lost
    if (Diesel.PAYLOAD == p.name) {
      root.engine.map(_.warning(EWarning("payload put in localCtx!! " + this.curNode.mkString)))
      base.foreach(_.put(p))
    } else {
      // overwrite here, don't propagate up
      attrs = p :: attrs.filter(_.name != p.name)
    }
  }

  override def remove(name: String): Option[P] = {
    attrs.find(a => a.name == name).map { p =>
      attrs = attrs.filter(_.name != name)
      // not removing from base
      p
    }
  }

  override def clear = {
    attrs = Nil
  }

  override def toString = this.getClass.getSimpleName + ":" + cur.mkString //+ "\n base: " +base.toString
}

