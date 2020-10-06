/*  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine.nodes

import razie.diesel.engine._
import razie.tconf.EPos

/** error and stop engine */
class EEngMsg(val msg: String, val details: String = "", val kind:String, val colorClass:String)
    extends CanHtml
    with HasPosition
    with InfoNode {

  var pos: Option[EPos] = None

  def withPos(p: Option[EPos]) = {
    this.pos = p; this
  }

  override def toHtml =
    if (details.length > 0)
      span(kind, colorClass, details, "style=\"cursor:help\"") + " " + msg
    else
      span(kind, colorClass, details) + " " + msg

  override def toString = kind + msg
}

/** error and stop engine */
case class EEngStop(override val msg: String, override val details: String = "")
    extends EEngMsg(msg, details, "error::", "danger")

/** suspend execution - presumably waiting for someone to continue this branch
  *
  * use onSuspend to start the async message  (like sending a DeRep) - you'll have control next
  */
case class EEngSuspend(override val msg: String, override val details: String = "", onSuspend: Option[(DomEngine,
    DomAst, Int) => Unit])
    extends EEngMsg(msg, details, "suspend::", "warning")

/** suspend execution - presumably waiting for someone to continue this branch
  *
  * use onSuspend to start the async message  (like sending a DeRep) - you'll have control next
  */
case class EEngSuspend(override val msg: String, override val details: String = "", onSuspend:Option[(DomEngine, DomAst, Int) => Unit])
    extends EEngMsg(msg, details, "suspend::", "warning")

/**
  * opposite of suspend - complete node
  */
case class EEngComplete(override val msg: String, override val details: String = "")
    extends EEngMsg(msg, details, "complete::", "warning")


