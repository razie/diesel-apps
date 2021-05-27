/*  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine.nodes

import razie.diesel.engine._
import razie.tconf.EPos


/** a generic scope demarcation, for things like error propag etc
  *
  * @param msg
  * @param scope the message generating this, if any. If diesel.try it would have more parms
  * @param details
  */
case class EScope(msg: String, scope: Option[EMsg], details: String = "") extends CanHtml with HasPosition with
    InfoNode {
  var pos: Option[EPos] = None

  def withPos(p: Option[EPos]) = {
    this.pos = p;
    this
  }

  override def toHtml =
    kspan("info", "default") +
        (
            if (details.length > 0)
              spanClick("scope::", "info", details) + msg
            else
              span("scope::", "info", details) + " " + msg
            )

  override def toString = "scope::" + msg + details
}

