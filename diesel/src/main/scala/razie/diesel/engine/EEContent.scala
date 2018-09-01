/**
  * ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package razie.diesel.engine

import java.util.regex.Pattern

import org.json.JSONObject
import razie.Snakk
import razie.diesel.dom.RDOM._
import razie.diesel.dom._
import razie.xp.JsonOWrapper

import scala.Option.option2Iterable
import scala.util.Try

/** a REST request or response: content and type */
class EEContent(
                 val body: String,
                 val contentType: String,
                 val headers: Map[String, String] = Map.empty,
                 val iroot: Option[Snakk.Wrapper[_]] = None,
                 val raw: Option[Array[Byte]] = None) {

  def isXml = contentType != null && (contentType startsWith "application/xml") || (contentType startsWith "text/xml")

  def isJson = contentType != null && (contentType startsWith "application/json")

  def asJsonPayload = {
    // sometimes you get empty
    val b = if (body.length > 0) body else "{}"
    P("payload", b, WTypes.JSON).withValue(new JSONObject(b), WTypes.JSON)
  }

  import razie.Snakk._

  /** xp root for either xml or json body */
  lazy val root: Snakk.Wrapper[_] = iroot getOrElse {
    contentType match {
      case _ if isXml => Snakk.xml(body)
      case _ if isJson => if (body.trim.startsWith("{")) Snakk.json(body) else Snakk.json("{}") // avoid exceptions parsing
      case x@_ => Snakk.json("{error: 'unknown SnakkCall.contentType'}")
      //throw new IllegalStateException ("unknown content-type: "+x)
    }
  }

  lazy val hasValues = if (isXml || isJson) root \ "values" else Snakk.empty

  lazy val r = if (hasValues.size > 0) hasValues else root

  // todo not optimal
  def exists(f: scala.Function1[P, scala.Boolean]): scala.Boolean = {
    val x = (r \ "*").nodes collect {
      case j: JsonOWrapper => {
        import scala.collection.JavaConversions._
        j.j.keys.map(_.toString).map { n =>
          P(n, r \ "*" \@@ n)
        }
      }
    }
    x.flatten.exists(f)
  }

  // todo don't like this
  def get(name: String): Option[String] = {
    (r \@@ name).toOption
  }

  /** name, default, expr
    *
    * will replace a.b.c with a/b/@c and a/b/c with a/b/@c
    */
  def ex(n: String, d: String, e: String, oe: Option[Expr]) = {
    def forceAttr(path: String) = path.replaceFirst("(.*)/([^/]*)$", "$1/@$2")

    if (e.isEmpty)
      (n, (r \@@ n OR d).toString)
    else if (e.startsWith("/") && e.endsWith("/")) // regex
      (n, e.substring(1, e.length - 1).r.findFirstIn(body).mkString)
    else oe match {
      case Some(xpi: XPathIdent) => (n, (r \@@ forceAttr(xpi.expr) OR d).toString)
      case Some(api: AExprIdent) => (n, (r \@@ forceAttr(api.expr.replaceAllLiterally(".", "/")) OR d).toString)
      case _ => (n, (r \@@ forceAttr(e.replaceAllLiterally(".", "/")) OR d).toString)
    }
  }

  /** extract the values and expressions from a response
    *
    * @param templateSpecs a list of name/expression
    * @param spec          a list of name/default/expression
    * @param regex         an optional regex with named groups
    */
  def extract(templateSpecs: Map[String, String], spec: Seq[(String, String, String, Option[Expr])], regex: Option[String], contentType: Option[String] = None) = {

    if (regex.isDefined) {
      // first try regex
      val rex =
        if (regex.get.startsWith("/") && regex.get.endsWith("/"))
          regex.get.substring(1, regex.get.length - 1)
        else regex.get

      val jrex = Pattern.compile(rex).matcher(body)
      //          val hasit = jrex.find()
      if (jrex.find())
        (
          for (g <- spec)
          // todo inconsistency: I am running rules if no mocks fit, so I should also run any executor ??? or only the isMocks???
            yield (
              g._1,
              Try {
                jrex.group(g._1)
              }.getOrElse(g._2)
            )
          ).toList
      else Nil

    } else if (templateSpecs.nonEmpty) {
      // did the template specify some extraction?
      val strs = templateSpecs.map { t =>
        ex(t._1, "", t._2, None)
      }
      strs.toList

    } else if (spec.nonEmpty) {
      // main case - use the message spec, if the message specified a return
      spec.map(t => ex(t._1, t._2, t._3, t._4)).filter(_._2.nonEmpty).toList

    } else if (isJson) {
      // last ditch attempt to discover some values
      Try {
        val res = jsonParsed(body)
        val a = res.getJSONObject("values")
        val strs = if (a.names == null) List.empty else (
          for (k <- 0 until a.names.length())
            yield (a.names.get(k).toString, a.getString(a.names.get(k).toString))
          ).toList
        strs.toList
      }.getOrElse(Nil)
    } else
      Nil
  }

  override def toString = s"Content-type:${this.contentType} Headers:${this.headers.mkString} \nBody: ${this.body}"
}

