/**
  *  ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.ext

import razie.diesel.engine.{DomAst, DomEngine, InfoNode}
import razie.tconf.EPos
import razie.wiki.Enc


object EErrorUtils {
  def ttos (t:Throwable) = {
    val sw = new java.io.StringWriter()
    val pw = new java.io.PrintWriter(sw)
    t.printStackTrace(pw)

    val s = sw.toString
    s
  }
}

/** some error, with a message and details */
case class EError(msg: String, details: String = "") extends CanHtml with HasPosition with InfoNode {
  def this(msg:String, t:Throwable) =
    this(
      Enc.escapeHtml(msg + t.toString),
      Enc.escapeHtml(EErrorUtils.ttos(t))
    ) // escape html - some exc contain html content

  var pos: Option[EPos] = None

  def withPos(p: Option[EPos]) = {
    this.pos = p; this
  }

  override def toHtml =
    kspan("err", "default") +
        (
            if (details.length > 0)
              spanClick("fail-error::", "danger", details) + msg
            else
              span("fail-error::", "danger", details) + " " + msg
            )

  override def toString = "fail-error::" + msg
}

/** some error, with a message and details */
case class EWarning(msg: String, details: String = "") extends CanHtml with HasPosition with InfoNode {
  def this(msg:String, t:Throwable) =
    this(
      Enc.escapeHtml(msg + t.toString),
      Enc.escapeHtml(EErrorUtils.ttos(t))
    ) // escape html - some exc contain html content

  var pos: Option[EPos] = None

  def withPos(p: Option[EPos]) = {
    this.pos = p; this
  }

  override def toHtml =
    if (details.length > 0)
      spanClick("warn::", "warning", details) + msg
    else
      span("warn::", "warning", details) + " " + msg

  override def toString = "fail-warn::" + msg
}


/** error and stop engine */
case class EEngStop(msg: String, details: String = "") extends CanHtml with HasPosition with InfoNode {

  var pos: Option[EPos] = None

  def withPos(p: Option[EPos]) = {
    this.pos = p; this
  }

  override def toHtml =
    if (details.length > 0)
      span("error::", "danger", details, "style=\"cursor:help\"") + " " + msg
    else
      span("error::", "danger", details) + " " + msg

  override def toString = "error::" + msg
}

/** suspend execution - presumably waiting for someone to continue this branch
  *
  * use onSuspend to start the async message  (like sending a DeRep) - you'll have control next
  */
case class EEngSuspend(msg: String, details: String = "", onSuspend:Option[(DomEngine, DomAst, Int) => Unit])
    extends CanHtml
    with    HasPosition
    with    InfoNode {

  var pos: Option[EPos] = None

  def withPos(p: Option[EPos]) = {
    this.pos = p; this
  }

  override def toHtml =
    if (details.length > 0)
      span("suspend::", "warning", details, "style=\"cursor:help\"") + " " + msg
    else
      span("suspend::", "warning", details) + " " + msg

  override def toString = "suspend::" + msg
}

/** a simple info node with a message and details - details are displayed as a popup */
case class EInfo(msg: String, details: String = "") extends CanHtml with HasPosition with InfoNode {
  var pos: Option[EPos] = None

  def withPos(p: Option[EPos]) = {
    this.pos = p; this
  }

  override def toHtml =
    if (details.length > 0) {
      spanClick(
        "info::",
        "info",
        details,
        msg
      )
    } else {
      span("info::", "info", details) + " " + msg
    }

  override def toString = "info::" + msg
}

/** duration of the curent op */
case class EDuration(millis:Long, msg: String="") extends CanHtml with InfoNode {

  override def toHtml = span("info::", "info") + s" $millis ms - $msg"

  override def toString = "info::" + s" $millis ms - $msg"
}


