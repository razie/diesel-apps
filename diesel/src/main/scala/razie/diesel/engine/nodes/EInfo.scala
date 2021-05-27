/*  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine.nodes

import razie.diesel.engine._
import razie.tconf.EPos
import razie.wiki.Enc
import scala.collection.mutable.ListBuffer


object EErrorUtils {
  val MAX_STACKTRACE_LINES = 30
  val MAX_STACKTRACE_LINES_PER = 10 // per exception

  /** throwable to string */
  def ttos (t:Throwable) = {
    val sw = new java.io.StringWriter()
    val pw = new java.io.PrintWriter(sw)
    t.printStackTrace(pw)

    // why always big stack traces? they're kind'a pointless
    val f = new ListBuffer[ListBuffer[String]]()
    sw.toString.lines.toList.zipWithIndex.collect {
      case t@(l,i) if i == 0 => {
        f.append(new ListBuffer[String]())
        f.last.append(l)
      }
      case t@(l,i) if l.contains("Caused by:") => {
        f.last.append("...")
        f.append(new ListBuffer[String]())
        f.last.append(l)
      }
      case t@(l,i) => {
        if(f.last.size < MAX_STACKTRACE_LINES_PER) f.last.append(l)
      }
    }
    f.flatten.mkString("\n")
  }
}

/** some error, with a message and details */
case class EError(msg: String, details: String = "", code: String = "ERROR", t: Option[Throwable] = None)
    extends CanHtml with HasPosition with InfoNode {

  def this(msg: String, t: Throwable) =
    this(
      Enc.escapeHtml(msg + ": " + t.getClass.getSimpleName + ": " + t.getMessage),
      Enc.escapeHtml(EErrorUtils.ttos(t)),
      "ERROR",
      Some(t)
    ) // escape html - some exc contain html content

  var pos: Option[EPos] = None
  var handled: Boolean = false

  def withPos(p: Option[EPos]) = {
    this.pos = p;
    this
  }

  def withCode(code: String) = {
    this.copy(code = code)
  }

  override def toHtml = {
    val getErrClass = if (handled) "warn" else "err"
    val color = if (handled) CanHtml.COLOR_WARN else CanHtml.COLOR_DANGER
    val spos = if (pos.isDefined) kspan("pos") else ""

    kspan(getErrClass, "default") +
        (
            if (details.length > 0)
              spanClick("fail-error::", color, details) + spos + msg
            else
              span("fail-error::", color, details) + spos + " " + msg
            )
  }

  override def toString = "fail-error::" + msg + details
}

/** some error, with a message and details */
case class EWarning(msg: String, details: String = "", code:String = "WARNING") extends CanHtml with HasPosition with InfoNode {
  def this(msg:String, t:Throwable) =
    this(
      Enc.escapeHtml(msg + t.toString),
      Enc.escapeHtml(EErrorUtils.ttos(t))
    ) // escape html - some exc contain html content

  var pos: Option[EPos] = None

  def withPos(p: Option[EPos]) = {
    this.pos = p;
    this
  }

  def withCode(code: String) = {
    this.copy(code = code)
  }

  override def toHtml = {
    val spos = if (pos.isDefined) kspan("pos") else ""
    if (details.length > 0)
      spanClick("warn::", "warning", details) + spos + msg.replace("\n", "")
    else
      span("warn::", "warning", details) + spos + " " + msg.replace("\n", "")
  }

  override def toString = "fail-warn::" + msg
}

/** a simple info node with a message and details - link opens */
case class ELink(msg: String, url: String = "") extends CanHtml with HasPosition with InfoNode {
  var pos: Option[EPos] = None

  def withPos(p: Option[EPos]) = {
    this.pos = p;
    this
  }

  override def toHtml =
    s"""<span onclick="welink('$url')" style="cursor:pointer" class="label
       |label-info" title="click to open">link</span>&nbsp; $msg""".stripMargin

  override def toString = "info::" + msg
}

/** a simple info node with a message and details - details are displayed as a popup */
case class ETrace(msg: String, details: String = "") extends CanHtml with HasPosition with InfoNode {
  var pos: Option[EPos] = None

  def withPos(p: Option[EPos]) = {
    this.pos = p;
    this
  }

  override def toHtml = {
    val spos = if (pos.isDefined) kspan("pos") else ""
    if (details.length > 0) {
      spanClick(
        "info::",
        "info",
        details,
        spos + msg.replace("\n", "")
      )
    } else {
      span("info::", "info", details) + spos + " " + msg
    }
  }

  override def toString = "info::" + shorten(msg, 200)
}

/** a simple info node with a message and details - details are displayed as a popup */
case class EInfo(msg: String, details: String = "") extends CanHtml with HasPosition with InfoNode {
  var pos: Option[EPos] = None

  def withPos(p: Option[EPos]) = {
    this.pos = p;
    this
  }

  override def toHtml = {
    val spos = if (pos.isDefined) kspan("pos") else ""
    if (details.length > 0) {
      spanClick(
        "info::",
        "info",
        details,
        spos + msg.replace("\n", "")
      )
    } else {
      span("info::", "info", details) + spos + " " + msg
    }
  }

  override def toString = "info::" + shorten(msg, 200)
}

/** a simple wrapper */
case class EInfoWrapper(a:Any) extends CanHtml with HasPosition with InfoNode {
  var pos: Option[EPos] = if(a.isInstanceOf[HasPosition]) a.asInstanceOf[HasPosition].pos else None

  override def toHtml =
    if (a.isInstanceOf[CanHtml]) a.asInstanceOf[CanHtml].toHtml
    else span("info::", "info", "") + " " + a.toString.replace("\n", "")

  override def toString = "info:: " + shorten(a.toString, 200)
}

/** duration of the curent op */
case class EDuration(millis:Long, msg: String="") extends CanHtml with InfoNode {

  override def toHtml = span("info::", "info") + s" $millis ms - $msg"

  override def toString = "info::" + s" $millis ms - $msg"
}

