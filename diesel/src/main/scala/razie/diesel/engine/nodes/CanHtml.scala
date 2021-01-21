/*  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine.nodes

import razie.tconf.EPos
import razie.wiki.Enc

/** some elements have a desired kind */
trait HasKind {
  def kind: Option[String]
}

/** elements that have a position in a spec - we can link to the spec line:col */
trait HasPosition {
  def pos: Option[EPos]

  /** key span with possible link. pass None to not have a link */
  def kspan(s: String, k: String = "default", overwritePos: Option[EPos] = Some(
    EPos.EMPTY), title: Option[String] = None, kind: Option[String] = None) = {
    val actualPos = if (overwritePos.exists(_.isEmpty)) pos else overwritePos

    def mkref: String = actualPos.map(_.toRef).mkString

    val t = title.map(CanHtml.prepTitle)
    val kin = kind.map(k => s"""kind="${k}"""").mkString

    actualPos.map(p =>
      s"""<span $kin posw="${p.wpath}" posr="${p.line}" onclick="$mkref" style="cursor:pointer" class="label
         |label-$k" ${t.mkString}>$s</span>&nbsp;""".stripMargin
    ) getOrElse
        s"""<span $kin class="label label-$k" ${t.mkString}>$s</span>&nbsp;"""
  }
}

/** html formatting utiliies */
object CanHtml {
  def prepTitle(title: String) = {
    val h = Enc.escapeHtml(title)
    val x = h.replaceAll("\\\"", "")
    //      val x = h.replaceAll("\\\"", "\\\"")
    val t = if (h.length > 0) s"""title="$x" """ else ""
    t
  }

  def span(s: String, k: String = "default", title: String = "") = {
    val t = prepTitle(title)
    s"""<span class="label label-$k" $t>$s</span>"""
  }

  val COLOR_INFO = "info"
  val COLOR_DANGER = "danger"
  val COLOR_WARN = "warning"
}

/** things that can represent themselves in HTML - instances have an toHtml method */
trait CanHtml {
  /** format an html keyword span
    *
    * @param s     the keyword
    * @param k     the color code or empty for no label class
    * @param title optional hover title
    * @param extra optional other attrs
    * @return
    */
  def span(s: String, k: String = "default", title: String = "", extra: String = "") = {
    val t = CanHtml.prepTitle(title)
    val cls = if (k.length > 0) s"""label label-$k""" else ""
    s"""<span class="$cls" $t $extra>$s</span>"""
  }

  /** format a clickable span, which dumps content
    *
    * @param s     the keyword
    * @param k     the color code or empty for no label class
    * @param title optional hover title
    * @param extra optional other attrs - shown on popup
    * @return
    */
  def spanClick(s: String, k: String = "default", title: String = "", extra: String = "") = {
    val id = java.util.UUID.randomUUID().toString
    span(
      s,
      k,
      title,
      s"""style="cursor:help" id="$id" onclick="dieselNodeLog($$('#$id').prop('title'));""""
    ) + " " + extra
  }

  /**
    * format an html message span
    *
    * wrap for EMsg where the kspan will wrap it anyways
    */
  def ea(e: String, a: String, title: String = "", wrap: Boolean = true) = {
    val t = CanHtml.prepTitle(title)
    (if (wrap) s"""<span class="label label-default" $t>""" else "") +
        s"""<span style="font-weight:bold; color:moccasin">$e</span>.<span
           |      class="" style="font-weight:bold; color:lightblue">$a</span>""".stripMargin +
        (if (wrap) """ </span>""" else "")
  }

  /** *
    * format an html element span
    */
  def token(s: String, title: String = "", extra: String = "") = {
    val t = CanHtml.prepTitle(title)
    s"""<span $t $extra>$s</span>"""
  }

  def tokenValue(s: String) =
    "<code>" + token(s, "value", """ class="string" """) + "</code>"

  def toHtml: String

  /** shorten string */
  def shorten(s: String, len: Int = 100) = {
    if (s.length > len) {
      s.take(len) + "..."
    } else {
      s
    }
  }
}

