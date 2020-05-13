/*  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

import java.util.regex.Pattern
import razie.Snakk
import razie.diesel.Diesel
import razie.diesel.dom.RDOM._
import razie.diesel.dom._
import razie.diesel.engine.exec.EESnakk
import razie.diesel.engine.nodes.flattenJson
import razie.diesel.expr.{AExprIdent, ECtx, Expr, SimpleECtx}
import razie.diesel.model.DieselMsg
import razie.xp.JsonOWrapper
import scala.Option.option2Iterable
import scala.collection.mutable.{HashMap, ListBuffer}
import scala.collection.parallel.mutable
import scala.util.Try

/** a REST request or response: content and type and processing thereof */
class EContent(
                 val body: String,
                 val contentType: String,
                 val code : Int = 200,
                 val headers: Map[String, String] = Map.empty,
                 val iroot: Option[Snakk.Wrapper[_]] = None,
                 val raw: Option[Array[Byte]] = None) {

  var warnings:List[Any] = Nil

  def isXml = contentType != null && (contentType startsWith "application/xml") || (contentType startsWith "text/xml")

  def isJson = contentType != null && (contentType startsWith "application/json")
  //|| contentType == null && b.trim.startsWith("{")

  def asJsonPayload = {
    // sometimes you get empty
    val b = if (body.length > 0) body else "{}"
    if(b.trim.startsWith("{")) P.fromTypedValue(Diesel.PAYLOAD, b, WTypes.JSON)
    else if(b.trim.startsWith("[")) P.fromTypedValue(Diesel.PAYLOAD, b, WTypes.ARRAY)
    else {
      val ex = new IllegalArgumentException("JSON object should start with { or [ but it starts with: " + body.take(5))
      P.fromTypedValue(Diesel.PAYLOAD, ex, WTypes.wt.EXCEPTION)
    }
  }

  /** parse incoming POST for a diesel request. it will incliude a typed PAYLOAD with the entire body */
  def asDieselParams (implicit ctx: ECtx): List[P] = {
    if(body.trim.startsWith("{")) {
      // flatten the incoming json
      asJsonPayload :: flattenJson(asJsonPayload)(ctx)
    }
    else if(body.trim.startsWith("[")) {
      // one array as payload
      List(asJsonPayload)
    } else {
      // POST content as string
      List(P.fromTypedValue(Diesel.PAYLOAD, body, WTypes.wt.STRING))
    }
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

  /** headers as a nice lowercase P */
  def headersp = EContent.headersp(headers)

  /** headers as a nice lowercase P */
  def httpCodep = P.fromTypedValue( EESnakk.SNAKK_HTTP_CODE, code)

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
  def getp(name: String): Option[P] = {
    (r \@@ name).toOption.filter(_.length > 0).map {v=>
      P.fromSmartTypedValue(name, v)
    }
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

  override def toString = s"Content-type:${this.contentType}\nHeaders:${this.headers.mkString("\n")}\nBody: ${this.body}"
}

/** content processing utils */
object EContent {

  /** apply the regex to body and extract any named groups */
  def extractRegexParms (rex:String, body:String): List[(String,String)] = {
    // stupid java can't give me the group names - let's find them
    val groupNames = "\\(\\?\\<([^<>]+)\\>".r.findAllIn(rex).map(_.replaceAll("[(?<>]", "")).toList
//    val groupNames = "\\<([^<>]+)\\>".r.findAllIn(rex).map(_.replaceAll("[<>]", "")).toList

    val jrex = Pattern.compile(rex).matcher(body)
    val groups3 = if (jrex.find()) {
      groupNames.map { n =>
      {
        Try {
          (n, jrex.group(n))
        }.getOrElse((n, "not found"))
      }
      }.toList
    } else Nil

    groups3
  }

  /** apply the path expr to body and extract any named groups */
  def extractPathParms (incoming:String, path:String): (Boolean, List[(String,String)]) = {
    // ? what's this?
    val a = path.replaceFirst("\\?.*", "").split("/")
    val b = incoming.replaceFirst("\\?.*", "").split("/")

    var matched = true
    var parms = new ListBuffer[(String,String)]()
    var i = 0
    val len = a.length min b.length
    while (i < len && matched) {
      val ai = a(i)
      val bi = b(i)
      if(ai == bi) matched = true
      else if (ai.startsWith(":")) {
        parms.append((ai.substring(1), bi))
      } else if (i == a.length-1 && ai.startsWith("*")) {
        parms.append((ai.substring(1), b.slice(i, b.length).mkString("/")))
        // last one is path?
        return (true, parms.toList)
      } else {
        return (false, Nil)
      }
      i += 1
    }

    // if lengths not matched, oops - the only way this works is with path* which was above
    if(i < a.size || i < b.size)
      matched = false

    (matched, parms.toList)
  }

  /** headers as a nice lowercase P */
  def headersp (headers:Map[String,String]) = {
    P.fromTypedValue(
      EESnakk.SNAKK_HTTP_HEADERS,
      headers.map{t=> (t._1.toLowerCase, t._2)}.toMap,
      WTypes.JSON)
  }

}

