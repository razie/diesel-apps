/*  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine.exec

import com.razie.pub.comms.{CommRtException, Comms}
import java.net.{URI, URL}
import razie.Snakk._
import razie.diesel.Diesel
import razie.diesel.Diesel.PAYLOAD
import razie.diesel.dom.RDOM._
import razie.diesel.dom.{DieselAssets, _}
import razie.diesel.engine.RDExt.{DieselJsonFactory, spec}
import razie.diesel.engine._
import razie.diesel.engine.nodes._
import razie.diesel.expr.{ECtx, SimpleExprParser}
import razie.diesel.snakk.FFDPayload
import razie.tconf.{DTemplate, EPos}
import razie.wiki.Enc
import razie.wiki.model.WID
import razie.{Logging, js}
import scala.Option.option2Iterable
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try

/** executor for snakking REST APIs */
class EESnakk extends EExecutor("snakk") with Logging {
  import EESnakk._

  /** can I execute this task? */
  override def test(ast: DomAst, m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) = {
    def known(s: String) =
      (s contains "GET") ||
          (s contains "PATCH") ||
          (s contains "POST") ||
          (s contains "TELNET") ||
          (s contains "HTTP")

    // snakk messages
    m.ea == "snakk.json" ||
        m.ea == "snakk.xml" ||
        m.ea == "snakk.text" ||
        m.ea == "snakk.telnet" ||
        m.ea == "snakk.ffd" ||
        m.ea == "snakk.ffdFormat" ||
        m.ea == "snakk.parse.xml"   ||
    m.ea == "snakk.parse.json"  ||
    m.ea == "snakk.parse.regex" ||
    // and also if the stypes are known and there are templates for them
    known(m.stype) ||
      spec(m).exists(m => known(m.stype) ||
          // todo big performance issue - looking up templates for each message, called from expandEMsg
        ctx.findTemplate(m.entity + "." + m.met).exists(x=>
          (x.tags.contains("request") ||
          x.tags.contains("response")) &&
              ! x.tags.contains("in")  // if tagged with in, don't match for out
        ))
  }

  /** execute for snakk.parse... */
  def parseApply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] = {
    val response = ctx.getRequired(Diesel.PAYLOAD)
    val ct = if (in.ea.contains("xml")) "application/xml" else "application/json"

    val reply = new EContent(response, ct, 200, Map.empty)

    // 2. extract values
    val strs = if(in.ea == "snakk.parse.regex") {
      val regex = ctx.getRequired("regex")
      EContent.extractRegexParms(regex, response).map(t => new P(t._1, t._2))
    } else if(in.ea == "snakk.parse.json") {
        reply.asJsonPayload :: Nil
    } else {
      P.fromSmartTypedValue(Diesel.PAYLOAD, new IllegalArgumentException("Can't parse xml yet")) :: Nil //.withPos(pos)
    }

    // add the resulting values
    strs.map { x =>
      if(x.isOfType(WTypes.wt.EXCEPTION)) {
        if(x.value.isDefined)
          EError(x.dflt).withPos(in.pos)
        else
          new EError(x.dflt, x.value.get.asInstanceOf[Throwable]).withPos(in.pos)
      } else {
        ctx.put(x)
        EVal(x).withPos(in.pos)
      }
    }
  }

  /** execute the snakk task then */
  override def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] = {

    if(in.ea startsWith  "snakk.parse.")
      return parseApply(in, destSpec)

    // FFD is separate
    if(in.ea == "snakk.ffd")
      return snakkFfd(in, destSpec)
    else if(in.ea == "snakk.ffdFormat")
      return formatFfd(in, destSpec)

    var filteredAttrs = in.attrs.filter(_.name != SNAKK_HTTP_OPTIONS)

    // flatten the options and add to filteredAttrs - that's convention with Comms
    filteredAttrs = snakkHttpOptions(in.attrs) ::: filteredAttrs

    // templates?
    val templateReq  = ctx.findTemplate(in.entity + "." + in.met, "request")
    val templateResp = ctx.findTemplate(in.entity + "." + in.met, "response")

    // reference to what was used to parse it - for debug navigation
    val pos = templateReq.map(_.pos).orElse{
      if(in.entity == "snakk") in.pos
      else (spec(in).flatMap(_.pos))
    }

    // expanded template content
    val formattedTemplate = templateReq.map(_.content).map { content =>
      prepStr2(content, in.attrs)
    }

    var eres = new InfoAccumulator()
    var response = "" // actual textual response
    var urlx = "?" // url, filled later for error rep

    var startMillis = System.currentTimeMillis()
    var durationMillis = 0L

    // this used for snakk.json etc
    def snakkWithCall(sc: SnakkCall) = {
      val newurl = EESnakk.relativeUrl(sc.url)

      // with templates

      val reply =

      // telnet

        if (templateReq.flatMap(_.parms.get("protocol")).exists(_ == "telnet") ||
            in.entity == "snakk" && in.met == "telnet") {

          eres += EInfo("Snakking TELNET ", Enc.escapeHtml(trimmed(sc.toJson, 5000))).withPos(pos)

          val REX = """([.\w]+)[:/ ](\w+).*""".r
          val REX(host, port) = newurl

          response = sc.telnet(host, port, sc.postContent, Some(eres))

          eres += ETrace("Response", Enc.escapeHtml(trimmed(response)))
          // todo error codes for telnet?
          val content = new EContent(response, sc.iContentType.getOrElse(""), 200, sc.iHeaders.getOrElse(Map.empty))
          content

        } else {   // http

          val x = url(
            prepUrl(stripQuotes(newurl),
              P("subject", in.entity) ::
                  P("verb", in.met) ::
                  filteredAttrs,
              Some(ctx)),
            (
                sc.headers ++ {
                  ctx.curNode.map(node =>
                    ("dieselNodeId" -> node.id)).toList.filter(x => !sc.headers.contains("dieselNodeId")) ++
                      ctx.root.asInstanceOf[DomEngECtx].settings.userId.map(x =>
                        ("dieselUserId" -> x)).toList.filter(x => !sc.headers.contains("dieselUserId")) ++
                      ctx.root.asInstanceOf[DomEngECtx].settings.configTag.map(x =>
                        ("dieselConfigTag" -> x)).toList.filter(x => !sc.headers.contains("dieselConfigTag"))
                }.toMap
                ),
            sc.method
          )

          urlx = x.toString
          sc.setUrl(x)

          trace("Snakking: " + trimmed(sc.toJson, 5000))
          trace("")
          trace("Snakking CURL: ")
          trace(trimmed(sc.toCurl, 5000))

          eres += EInfo("Snakking " + x.toString, Enc.escapeHtml(trimmed(sc.toCurl))).withPos(pos)

          response = sc.body // make the call

          if (response.length >= Comms.MAX_BUF_SIZE)
            eres += EError(s"BUF_SIZE - read too much from socket (${response.length} + bytes!)", "")

          trace("Snakk RESPONSE: " + trimmed(response))

          val content = Try {
            new EContent(
              response,
              sc.iContentType.getOrElse(""),
              sc.icode.getOrElse(-1),
              sc.iHeaders.getOrElse(Map.empty),
              sc.root)
          }.recover {
            case e:Throwable => {
              val c = new EContent(
                response,
                sc.iContentType.getOrElse(""),
                sc.icode.getOrElse(-1),
                sc.iHeaders.getOrElse(Map.empty),
                None)
              c.warnings = List(EWarning("Parsing", html(e.getLocalizedMessage)))
              c
            }
          }.get

          content.warnings.foreach(eres.append)
          eres += ETrace("Response", html(content.toString))
          content
        }

      durationMillis = System.currentTimeMillis() - startMillis
      eres += EDuration(durationMillis)

      // PROCESS the reply

      // does either template have return parm specs?
      val templateSpecs = (templateReq.map(_.parms).getOrElse(Map.empty) ++ templateResp.map(_.parms).getOrElse(
        Map.empty))
          .filter(_._1 != "content-type")
          .filter(_._1 != "signature")

      // message specified return mappings, if any
      val retSpec = if (in.ret.nonEmpty) in.ret else spec(in).toList.flatMap(_.ret)
      // collect any parm specs
      val specs = retSpec.map(p => (p.name, p.currentStringValue, p.expr.mkString, p.expr))

      val regex = templateSpecs.find(_._1 == "regex").map(_._2).orElse(specs.find(_._1 == "regex").map(_._3))

      // 1. response template specified

      // 2. extract values
      val strs = templateResp.map { tresp =>
        reply.extract(templateSpecs, specs, regex).map(t => new P(t._1, t._2))
      }.getOrElse(
        if (in.entity == "snakk" && in.met == "json") {
          reply.asJsonPayload :: Nil
        } else
          reply.extract(templateSpecs, specs, regex).map(t => new P(t._1, t._2))
      )

      // add the resulting values
      eres += strs.map { x =>
        if(x.isOfType(WTypes.wt.EXCEPTION)) {
          if(x.value.isDefined)
            EError(x.dflt).withPos(pos)
          else
            new EError(x.dflt, x.value.get.asInstanceOf[Throwable]).withPos(pos)
        } else {
          ctx.put(x)
          EVal(x).withPos(pos)
        }
      }

      // 3. look for stiching dieselTrace
      if (reply.isJson && reply.body.contains(DieselTrace.dieselTrace)) {
        val mres = js.parse(response)
        if (mres.contains(DieselTrace.dieselTrace)) {
          val trace = DieselJsonFactory.trace(mres(DieselTrace.dieselTrace).asInstanceOf[collection.Map[String, Any]])
          eres += trace
        }
      }

      val traceId = reply.headers.get("dieselFlowId").map { t =>
        var wid = WID("DieselEngine", t)
        val host = reply.headers.get("dieselHost")
        if (host.isDefined) {
          wid = wid.withSourceUrl(host.mkString)
        }

        val url = DieselAssets.mkEmbedLink(wid)
        val href = DieselAssets.mkAhref(wid)

        ELink(
          s"dieselFlow spawned $href",
          url)
      }.toList

      // if no typed result, add a generic text
      if (eres.eres.collect {
        case EVal(p) if p.name == PAYLOAD => p
      }.isEmpty) {
        eres += new EVal(PAYLOAD, reply.body)
      }

      // make sure payload is last
      eres.eres.filter {
        case v@EVal(p) if p.name == PAYLOAD => false
        case x@_ => true
      } :::
          new EInfo(SNAKK_RESPONSE, reply.body) ::
          new EVal(reply.httpCodep).withKind(AstKinds.DEBUG) ::
          new EVal(reply.headersp).withKind(AstKinds.DEBUG) ::
          traceId :::
          eres.eres.collect {
            case v@EVal(p) if p.name == PAYLOAD => v
          }
    }

    // no snakk call / template found
    def snakkWithoutCall() = {

      def findin(name: String) =
        in
            .attrs
            .find(_.name == name)
            .orElse(
              spec(in)
                  .flatMap(_.attrs.find(_.name == name))
                  .map(p => p.copy(
                    dflt = p.calculatedValue)) // if it's the spec - nobody calculates its value, could be CExpr
            )

      // no template or tcontent => old way without templates
      findin("url").map { u =>
        // is it relative?
        val newurl = if (u.currentStringValue startsWith "http") u.currentStringValue else
          ctx.root.asInstanceOf[DomEngECtx].settings.dieselHost.mkString.mkString + u.currentStringValue

        val sc = new SnakkCall("http", in.arch, newurl, Map.empty, "")
        //          case class SnakkCall (method:String, url:String, headers:Map[String,String], content:String) {
        val ux = url(
          prepUrl(stripQuotes(newurl),
            P("subject", in.entity) ::
                P("verb", in.met) ::
                filteredAttrs,
            Some(ctx))
        )

        urlx = ux.toString
        sc.setUrl(ux)

        val content: EContent = {
          if (sc.method == "open") {
            val response = sc.telnet("localhost", "9000", sc.postContent, Some(eres))
            new EContent(sc.body, "application/text")
          } else {
            eres += EInfo("Snakking " + urlx, Enc.escapeHtml(trimmed(sc.toJson, 5000))).withPos(pos)
            val response = sc.body
            new EContent(sc.body, sc.iContentType.getOrElse(""), sc.icode.getOrElse(-1), sc.iHeaders.getOrElse(Map.empty), sc.root)
          }
        }

        durationMillis = System.currentTimeMillis() - startMillis
        eres += EDuration(durationMillis)

        eres += ETrace("Response", html(content.toString))

        // 2. extract values
        val x = if (in.ret.nonEmpty) in.ret else spec(in).toList.flatMap(_.ret)
        val specs = x.map(p => (p.name, p.currentStringValue, p.expr.mkString, p.expr))
        val regex = x.find(_.name == "regex") orElse findin("regex") map (_.currentStringValue)
        val strs = content.extract(Map.empty, specs.filter(_._1 != "regex"), regex)

        // add the resulting values
        eres.eres ::: strs.map(t => new P(t._1, t._2)).map { x =>
          ctx.put(x)
          EVal(x).withPos(pos)
        } :::
            new EVal(SNAKK_RESPONSE, content.body).withKind(AstKinds.TRACE) ::
            new EVal(content.httpCodep).withKind(AstKinds.TRACE) ::
            new EVal(content.headersp).withKind(AstKinds.TRACE) ::
            // todo here's where i would add the response headers - make the snakk.response an object?
            new EVal(PAYLOAD, content.body) ::
            Nil
      } getOrElse
          // need to create a val - otherwise DomApi.rest returns the last Val
          EError("no url attribute for RESTification - and no request template found") ::
              EVal(P("snakk.error", "no url attribute for RESTification - and no request template found")) ::
              Nil
    }

    try {

      // 1. prepare the request and make the call

      // do we have a known template OR snakk call?
      val osc =
        if(in.entity == "snakk" && (in.met == "json" || in.met == "xml" || in.met=="text" ))
          Some(scFromMsg(in.attrs))
        else if(in.entity == "snakk" && in.met == "telnet")
          Some(scFromMsgTelnet(in.attrs))
      else
          formattedTemplate.map(parseTemplate(templateReq, _, in.attrs, Some(ctx)))

      osc.map { sc =>

        snakkWithCall(sc)

      } getOrElse {

        snakkWithoutCall()
      }
    } catch {
      case t: Throwable => {
        caughtSnakkException(t,  httpOptions(in.attrs), response, startMillis, eres)
      }
    }
  }

  override def toString = "$executor::snakk "

  override val messages: List[EMsg] =
    EMsg("snakk", "ffd") ::
    EMsg("snakk", "json") ::
    EMsg("snakk", "text") ::
        EMsg("snakk", "xml") ::
        EMsg("snakk", "parse.xml") ::
        EMsg("snakk", "parse.json") ::
    EMsg("snakk", "parse.regex") ::
      Nil
}

/** snakk REST and formatting/parsing utilities */
object EESnakk {

  final val SNAKK_RESPONSE = "snakk.response"
  final val SNAKK_HTTP_OPTIONS = "snakkHttpOptions"
  final val SNAKK_HTTP_CODE = "snakkHttpCode"
  final val SNAKK_HTTP_HEADERS = "snakkHttpHeaders"
  final val SNAKK_HTTP_RESPONSE = "snakkHttpResponse"

  private def trimmed(s:String, len:Int = 2000) = (if(s != null && s.length > len) s"(>$len):\n" else "\n") + s.take(len)
  def html(s:String, len:Int = 2000) = Enc.escapeHtml(trimmed(s, len))

  /** parse a template into a SNakkCall
    *
    * {{template rest.create}}
    * POST /diesel/fiddle/react/rest/mcreate?name=$name&address=$address&resultMode=json
    * sso_token : joe 123456
    * content-type: application/json
    * *
    * {
    * "p1" : 1,
    * "p2" : 2
    * }
    * {{/template}}
    *
    * @param is
    * @return
    */
  def parseTemplate(t: Option[DTemplate], is: String, attrs: Attrs, ctx:Option[ECtx]=None) : SnakkCall = {
    if (t.flatMap(_.parms.get("protocol")).exists(_ == "telnet"))
      parseTelnetTemplate(t, is, attrs,ctx)
    else parseHttpTemplate(t, is, attrs,ctx)
  }

  def parseTelnetTemplate(t: Option[DTemplate], is: String, attrs: Attrs, ctx:Option[ECtx]) : SnakkCall = {
    // templates start with a \n
    var xis = is.replaceAll("\r", "")
    val s = if (xis startsWith "\n") xis.substring(1) else is
    val REX = """(?s)(\w+) ([.\w]+)[:/ ](\w+).*""".r
    val REX(verb, host, port) = s
    // need to leave the \n in place and s.lines will merge them
    val content = s.replaceFirst(".*\n", "")
    new SnakkCall("telnet", "telnet", s"$host:$port", Map.empty,
      content) //prepStr(content, attrs, ctx).replaceFirst("(?s)\n\r?$", ""))
  }

  def parseHttpTemplate(template: Option[DTemplate], is: String, attrs: Attrs, ctx:Option[ECtx]) : SnakkCall = {
    // templates start with a \n
    var xis = is.replaceAll("\r", "")
    val s = if (xis startsWith "\n") xis.substring(1) else xis
    val verb = s.split(" ").head

    val u = s.split(" ",2)
      .last
      .replaceFirst("(?s)( HTTP.*)?\n.*", "")
      .replaceAllLiterally(" ", "%20") // lame encode - maybe it's already encoded...

    val url = prepUrl(u.replaceFirst("(?s)\n.*", ""), attrs, ctx)

    val headers = if (s matches "(?s).*\n\r?\n\r?.*") s.replaceFirst("(?s)\n\r?\n\r?.*", "").lines.drop(1).mkString("\n") else ""
    var content = if (s matches "(?s).*\n\r?\n\r?.*") s.replaceFirst("(?s).*\n\r?\n\r?", "") else ""
    if (content endsWith ("\n")) content = content.substring(0, content.length - 1)

    val hattr = prepStr(headers, attrs, ctx).lines.map(_.split(":", 2)).collect {
      case a if a.size == 2 => (a(0).trim, a(1).trim)
    }.toSeq.toMap

    // only expand expressions if it's not a request template
    if(template.flatMap(_.parm("signature")).exists(_ != "request"))
      content = prepStr(content, attrs, ctx).replaceFirst("(?s)\n\r?$", "")

    new SnakkCall(
      "http",
      verb,
      url,
      hattr,
      content,
      template
    )
  }

  def relativeUrl(u:String)(implicit ctx:ECtx) =
    if (u startsWith "/")
      ctx.root.settings.dieselHost.mkString.mkString +
          u
    else u

  /** extract a snakk call from message args - good for snakk.json and snakk.xml */
  def scFromMsg(attrs: Attrs)(implicit ctx:ECtx) : SnakkCall = {

    def f(name:String, dflt:String="") = {
      attrs.find(_.name == name).map(_.calculatedValue).getOrElse(dflt)
    }

    // the values are already prepped and expanded - no special processing

    val verb = f("verb", "GET")
    val url = f("url")

//    val xurlStr = "http://www.example.com/CEREC® Materials & Accessories/IPS Empress® CAD.pdf"
    val xurl= new URL(relativeUrl(url));
    val xuri = new URI(xurl.getProtocol(), xurl.getUserInfo(), xurl.getHost(), xurl.getPort(), xurl.getPath(), xurl.getQuery(), xurl.getRef());

    val encurl = xuri.toASCIIString();

    val C = "url,verb,body,result,snakkHttpOptions".split(",")
    var headers = attrs.filter(p => !(C contains p.name))
    val fbody = attrs.find(_.name == "body")
    var content = f("body")
    content = content

    // figure out content type ?
    if (!attrs.exists(_.name.toLowerCase == "content-type")) {
      if (fbody.exists(_.ttype == WTypes.JSON))
        headers = P("Content-Type", "application/json") :: headers
      else if (fbody.exists(_.ttype == WTypes.XML))
        headers = P("Content-Type", "application/xml") :: headers
    }

    headers = snakkHttpOptions(attrs) ::: headers

    val hattr = headers.map(p => (p.name, p.calculatedValue)).toSeq.toMap

    new SnakkCall(
      "http",
      verb,
      encurl,
      hattr,
      content
    )
  }

  def httpOptions (attrs:Attrs)(implicit ctx: ECtx) : Map[String,Any] =
    attrs
        .find(_.name == EESnakk.SNAKK_HTTP_OPTIONS)
        .map(_.calculatedP)
        .flatMap(_.value)
        .map(_.asJson.toMap)
        .getOrElse(Map.empty)

  // flatten the options and add to filteredAttrs/headers - that's convention with Comms
  def snakkHttpOptions (attrs:Attrs)(implicit ctx: ECtx) = {
    httpOptions(attrs).map {t=>
      val s = t._2.toString
      P("snakkHttpOptions." + t._1, s)
    }.toList
  }


  /** extract a snakk call from message args - good for snakk.json and snakk.xml */
  def scFromMsgTelnet(attrs: Attrs)(implicit ctx:ECtx) : SnakkCall = {

    def f(name:String, dflt:String="") = {
      attrs.find(_.name == name).map(_.calculatedValue).getOrElse(dflt)
    }

    // the values are already prepped and expanded - no special processing

    val host = f("host")
    val port = f("port")
    var content = f("body")
    content = content

    new SnakkCall(
      "telnet",
      "telnet",
      s"$host:$port",
      Map.empty,
      content)
  }

  /** does template match url and verb - used to match incoming http to configured templates
    *
    * @param t template to test
    * @param verb verb to test, use "*" for any
    * @param path
    * @param content
    * @return
    */
  def templateMatchesUrl (t:DTemplate, verb:String, path:String, content:String): Option[Map[String,String]] = {
    val c = content.replaceFirst("^\n", "") // multiline template start with \n

    val v = c.split(" ").head
    var url = c.split(" ").tail.head.replaceFirst("(?s)\\?.*", "").replaceFirst("(?s)\n.*", "")
    val tpath = c.split("\\?").head

    // remove known prefixes for URL
    if(url.startsWith("/diesel/")) {
      url = url.replaceFirst("/diesel/(mock|rest|wreact/[^/]+/react)", "")
    }

    // split in segments and match them - each segment to be the same or a variable

    val t = url.split("[/.]")
    val p = path.split("[/.]") // todo I'm doing this because some places rely on react/class/action works as well as react/class.action

    val zipped = t.zip(p)
    val res = t.size == p.size && zipped.size == t.size && zipped.foldLeft(true)((x,y) => x && (y._1 == y._2 || y._1.startsWith("$")))

    // find the pairs
    // todo this is not used - put them in the engine's main context?
    val map =
      if(res)
        Some(
          t
              .zip(p)
              .foldLeft(List.empty[(String,String)])((a, b) => if(b._1.matches("""\$\{([^\}]*)\}""")) (b._1, b._2) :: a else a )
              .map (r=> (r._1.replaceAll("[${}]", ""), r._2))
              .toMap
        )
      else
        None

    map
  }

  val BRACKET_EXPR_PAT = """\$\{([^\}]*)\}""".r

  /** prepare the template content - expand ${parm} expressions - WITH SUPPORT FOR SIMPLE EXPRESSIONS */
  def prepStr2(content: String, attrs: Attrs, allowSimpleExpansion:Boolean = true)(implicit ctx:ECtx) = {
    // what style is used?
    if(content contains "${") {
      val s1 = BRACKET_EXPR_PAT.replaceAllIn(content, { m =>
        (new SimpleExprParser).parseExpr(m.group(1)).map {e=>
          P ("x", "", WTypes.wt.EMPTY, Some(e)).calculatedValue
        } getOrElse
          s"{ERROR: ${m.group(1)}"
      })
      s1
    } else if(allowSimpleExpansion)
      prepStr(content, attrs, Some(ctx))
    else
      content // no expansion
  }

  /** prepare the template content - expand $parm expressions */
  def expandExpr(url: String, attrs: Option[Attrs], ctx:Option[ECtx]):String = {
    //    val eeEscaped = es.replaceAllLiterally("(", """\(""").replaceAllLiterally(")", """\)""")

    var u = ""
    try {
      u = BRACKET_EXPR_PAT.replaceSomeIn(url, { m =>
        val n = m.group(1)
        attrs.flatMap(_.find(_.name == n)).orElse(ctx.flatMap(_.getp(n))).map(x =>
          ctx.map { implicit ctx =>
            stripQuotes(x.calculatedValue)
          }.getOrElse {
            stripQuotes(x.currentStringValue)
          }
        )
      })
    } catch {
      case e : Exception => throw new IllegalArgumentException(s"REGEX err for $url ").initCause(e)
    }

    u
  }

  /** prepare the template content - expand $parm expressions */
  def prepStr(url: String, attrs: Attrs, ctx:Option[ECtx]) = {
    //todo add expressions like ${...}
    val PATT = """(\$\w+)*""".r
    val u = PATT.replaceSomeIn(url, { m =>
      val n = if (m.matched.length > 0) m.matched.substring(1) else ""
      attrs.find(_.name == n).orElse(ctx.flatMap(_.getp(n))).map(x =>
        ctx.map{implicit ctx=>
          stripQuotes(x.calculatedValue)
        }.getOrElse{
          stripQuotes(x.currentStringValue)
        }
      )
    })
    u
  }

  /** prepare the URL - expand $parm expressions */
  def prepUrl(url: String, attrs: Attrs, ctx:Option[ECtx]) : String = {
    val u = expandExpr(url, Some(attrs), ctx)

    val res =
      if(u contains "$") // in case we parse template /cust/$id/...
        u
    else
        new URI(u).toString()

    res
  }

  def formatTemplate (content:String, ctx:ECtx) = {
    // todo either this or prepUrl not both
    var s1 = expandExpr(content, None, Some(ctx))

    s1 = BRACKET_EXPR_PAT.replaceSomeIn(s1, { m =>
      val n = m.group(1)
      if(n.length > 1)
        ctx.get(n) orElse Some("") // orElse causes $msg to expand to nothing if no msg
      else None
//      if(n.length > 1) ctx.get(n) else None
    })
    s1
  }

  /** parse FFD input string with a schema
    *
    * $msg snakk.ffd (x, schema="[[Spec:myschema]]", result="authMsg")
    *
    */
  def snakkFfd(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] = {
    val schema = in.attrs.find(_.name == "schema").map{s=>
      s.currentStringValue.replaceAll("\\[\\]", "")
    }

    val output = in.attrs.find(_.name == "result").map(_.currentStringValue)

    // should parse just one in put parm, but we can parse many and aggregate the results
    in.attrs.filter(p=> p.name != "schema" && p.name != "result").headOption.toList.flatMap {input=>
      val parsed = new FFDPayload(input.currentStringValue, schema.mkString).parse

      val results = parsed.getResult.map {values=>
        val x = new mutable.HashMap[String,Any]
        values.map{ p=>
          x.put(p.name, p.currentStringValue)
        }

        x.toMap
      }

      results.toList.flatMap {res=>
        output.toList.map(name=>P.fromTypedValue(name, res, WTypes.JSON)) :::
          EVal(P.fromTypedValue(PAYLOAD, res, WTypes.JSON)) :: Nil
      } ::: parsed.getErrors.map(EError(_))
    }
  }

  /** format FFD output string with a schema
    *
    * $msg snakk.ffd (x, schema="[[Spec:myschema]]", result="authMsg")
    *
    */
  def formatFfd(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] = {
    val schema = in.attrs.find(_.name == "schema").map{s=>
      s.currentStringValue.replaceAll("\\[\\]", "")
    }

    val output = in.attrs.find(_.name == "result").map(_.currentStringValue)

    val parms = in.attrs.filter(p=> p.name != "schema" && p.name != "result")
    val results = new FFDPayload("", schema.mkString).build(ctx)

    results.getResult.toList.flatMap {res=>
      output.toList.map(name=>P(name, res)) :::
        EVal(P(PAYLOAD, res)) :: Nil
    } ::: results.getErrors.map(EError(_))
  }

  override def toString = "$executor::snakk "

  def caughtSnakkException (t:Throwable, httpOptions:Map[String, Any], response:String, startMillis:Long, eres:InfoAccumulator) = {
    var code = -1
    var errContent = ""

    val cause =
      if(t.getCause != null &&
          (t.getCause.isInstanceOf[java.lang.IllegalArgumentException] ||
              t.getCause.isInstanceOf[java.net.SocketTimeoutException] ||
              t.getCause.isInstanceOf[java.net.ConnectException] ||
              t.getCause.getClass.getCanonicalName.startsWith("java.net.")
              // why not all java.net ex - no point remembering the stack traces
              )
      ) {
        razie.Log.log("error snakking: " + t.getClass.getName + " : " + t.getMessage + " cause: " + t.getCause.getMessage)
        eres += new EError("Exception: ", t.getCause.getMessage) :: Nil
        t.getCause
      } else if( t.isInstanceOf[CommRtException] && t.asInstanceOf[CommRtException].httpCode > 0 ) {
        razie.Log.log("error snakking: " + t.getClass.getName + " : " + t.getMessage);
        code = t.asInstanceOf[CommRtException].httpCode
        errContent = t.asInstanceOf[CommRtException].details

        val respCode = httpOptions.get("responseCode").map(_.toString)

        if(respCode.exists(x=> x == "*" || x.toInt == code)) {
          eres += new EInfo(
            "Warn - accepted code: " + Enc.escapeHtml(t.getMessage),
            Enc.escapeHtml(t.asInstanceOf[CommRtException].details)
          ) :: Nil
        } else {
          eres += new EError("Exception: " + Enc.escapeHtml(t.getMessage),
            Enc.escapeHtml(t.asInstanceOf[CommRtException].details)
          ) :: Nil
        }
        t
      } else {
        razie.Log.log("error snakking", t)
        eres += new EError("Exception: ", t) :: Nil
        t
      }

    val durationMillis = System.currentTimeMillis() - startMillis
    eres += EDuration(durationMillis)

    eres += ETrace("Response: ", html(response)) :: Nil

    if(code > 0) eres += EVal(P.fromTypedValue(EESnakk.SNAKK_HTTP_CODE, code)) :: Nil
    if(errContent.length > 0) eres += EVal(P.fromTypedValue(EESnakk.SNAKK_HTTP_RESPONSE, errContent)) :: Nil

    if(t.isInstanceOf[CommRtException]) {
      val uc = t.asInstanceOf[CommRtException].uc
      val x = uc.getHeaderFields.keySet().toArray.toList
      val headers = x.filter(_ != null).map(x => (x.toString, uc.getHeaderField(x.toString))).toMap
      eres += EVal(EContent.headersp(headers)).withKind(AstKinds.DEBUG)
    }

    eres += new EVal(PAYLOAD, "")

    eres +=
        // need to create a val - otherwise DomApi.rest returns the last Val
        EVal(P("snakkError", html(cause.getMessage, 10000))) ::
            Nil
  }

}


// to collect msg def
case class PCol(p: P, pos: Option[EPos])

case class PMCol(pm: PM, pos: Option[EPos])

case class MsgCol(e: String,
                  a: String,
                  pos: Option[EPos],
                  in: ListBuffer[PCol] = new ListBuffer[PCol],
                  out: ListBuffer[PCol] = new ListBuffer[PCol],
                  cons: ListBuffer[PMCol] = new ListBuffer[PMCol]
                 ) {
  def toHtml = EMsg(e, a).withPos(pos).toHtmlInPage
}
