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

/** static context does not allow changing values - good for evaluating expressions
  *
  * @param cur
  * @param base
  * @param curNode
  */
class StaticECtx(
  cur: List[P] = Nil,
  base: Option[ECtx] = None,
  curNode: Option[DomAst] = None) extends SimpleECtx(cur, base, curNode) {

  // check for overwriting values
  // todo if i'm in a message, this will always be the case - why am I alarming?
  // todo and who can see it? it can't be in the logs...
  def check(p: P): Option[EWarning] = {
    if (cur.exists(_.name == p.name)) {
      Some(EWarning("WARNING_OVERWRITE at put - you may be overwriting ide-effects: " + p.name))
    } else None
  }

  //todo should I throw up if no base?
  override def put(p: P): Unit = {
    if (p.name == Diesel.PAYLOAD) base.map(_.put(p))
    else throw new DieselExprException("CAN'T OVERWRITE STATIC CTX VARS")
//    check(p)
//     propagate to base, so it lives
//    base.map(_.put(p))
  }

  override def putAll(p: List[P]): Unit = {
    p.map(put) // overwrite logic
  }

  override def remove(name: String): Option[P] = {
    if (name == Diesel.PAYLOAD) base.flatMap(_.remove(name))
    else throw new DieselExprException("CAN'T OVERWRITE STATIC CTX VARS [remove]")
  }

  override def clear = {
    throw new DieselExprException("CAN'T OVERWRITE STATIC CTX VARS [clear]")
//    base.map(_.clear)
  }

  override def toString = this.getClass.getSimpleName + ":" + cur.mkString //+ "\n base: " +base.toString
}


/** passthrough to parent, but also overwrite currents, so behavior is as expected
  *
  * @param cur
  * @param base    - parent context
  * @param curNode - current Ast node
  */
class PassthroughECtx(
  cur: List[P] = Nil,
  base: Option[ECtx] = None,
  curNode: Option[DomAst] = None) extends SimpleECtx(cur, base, curNode) {

  // copy overriding values from base - this is important when values are overwritten and the ctx is re-created
//  attrs = base.toList.flatMap(_.asInstanceOf[SimpleECtx].attrs.filter { p =>
//    cur.exists { c => c.name == p.name }
//  }) // attrs must have been calculated...

  //todo should I throw up if no base?
  override def put(p: P): Unit = {
    // some parms propagate up - CANNOT duplicate here or updates to the upper version are lost
    if (Diesel.PAYLOAD == p.name) {
      base.foreach(_.put(p))
    } else {
      // also overwrite here, if exists
      if (attrs.exists(_.name == p.name)) attrs = p :: attrs.filter(_.name != p.name)
//     propagate to base, so it lives
      base.map(_.put(p))
    }
  }

  override def putAll(p: List[P]): Unit = {
    // reuse logic in put
    p.foreach(put)
  }

  override def remove(name: String): Option[P] = {
    base.flatMap(_.remove(name))
    val resp = attrs.find(x => name == x.name)
    attrs = attrs.filter(x => name != x.name)
    resp
  }

  override def clear = {
    base.map(_.clear)
  }

  override def toString = this.getClass.getSimpleName + ":" + cur.mkString //+ "\n base: " +base.toString
}
