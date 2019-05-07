/**
  * ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package mod.diesel.model.exec

import java.net.{URI, URL}

import com.razie.pub.comms.Comms
import razie.Snakk._
import razie.diesel.dom.RDOM._
import razie.diesel.dom._
import razie.diesel.engine.RDExt.{DieselJsonFactory, spec}
import razie.diesel.engine.{DomEngECtx, EEContent, InfoAccumulator}
import razie.diesel.exec.SnakkCall
import razie.diesel.ext.{MatchCollector, _}
import razie.diesel.snakk.FFDPayload
import razie.tconf.DTemplate
import razie.wiki.Enc
import razie.wiki.parser.SimpleExprParser
import razie.{clog, js}

import scala.Option.option2Iterable
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** snakk REST APIs */
class EESnakk extends EExecutor("snakk") {
  import EESnakk._

  private def trimmed(s:String, len:Int = 2000) = (if(s.length > len) s"(>$len):\n" else "\n") + s.take(len)

  /** can I execute this task? */
  override def test(m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) = {
    def known (s:String) =
      (s contains "GET") ||
      (s contains "PATCH") ||
      (s contains "POST") ||
      (s contains "TELNET") ||
      (s contains "HTTP")

    m.entity == "snakk" && m.met == "json"      ||
    m.entity == "snakk" && m.met == "xml"       ||
    m.entity == "snakk" && m.met == "text"      ||
    m.entity == "snakk" && m.met == "telnet"    ||
    m.entity == "snakk" && m.met == "ffd"       ||
    m.entity == "snakk" && m.met == "ffdFormat" ||
    known(m.stype) ||
      spec(m).exists(m => known(m.stype) ||
        ctx.findTemplate(m.entity + "." + m.met).exists(x=>
          x.parmStr.startsWith("request") ||
          x.parmStr.startsWith("response")
        ))
  }

  /** execute the task then */
  override def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] = {

    // FFD is separate
    if(in.entity == "snakk" && in.met == "ffd")
      return snakkFfd(in, destSpec)
    else if(in.entity == "snakk" && in.met == "ffdFormat")
      return formatFfd(in, destSpec)

    // templates?
    val templateReq  = ctx.findTemplate(in.entity + "." + in.met, "request")
    val templateResp = ctx.findTemplate(in.entity + "." + in.met, "response")

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

    try {

      // 1. prepare the request and make the call

      // do we have a known template OR snakk call?
      val osc =
        if(in.entity == "snakk" && (in.met == "json" || in.met == "xml" || in.met=="text"))
          Some(scFromMsg(in.attrs))
        else if(in.entity == "snakk" && in.met == "telnet")
          Some(scFromMsgTelnet(in.attrs))
      else
          formattedTemplate.map(parseTemplate(templateReq, _, in.attrs, Some(ctx)))

      osc.map { sc =>

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

            eres += EInfo("Response", Enc.escapeHtml(trimmed(response)))
            val content = new EEContent(response, sc.iContentType.getOrElse(""), sc.iHeaders.getOrElse(Map.empty))
            content

          } else {   // http

            val x = url(
              prepUrl(stripQuotes(newurl),
                P("subject", in.entity) ::
                P("verb", in.met) ::
                in.attrs,
                Some(ctx)),
              //            in.attrs.filter(_.name != "url").map(p=>(p.name -> p.dflt)).toMap,
              (
                sc.headers ++ {
                  ctx.curNode.map(node =>
                    ("dieselNodeId" -> node.id)).toList.filter(x=> ! sc.headers.contains("dieselNodeId")) ++
                    ctx.root.asInstanceOf[DomEngECtx].settings.userId.map(x =>
                      ("dieselUserId" -> x)).toList.filter(x=> ! sc.headers.contains("dieselUserId")) ++
                    ctx.root.asInstanceOf[DomEngECtx].settings.configTag.map(x =>
                      ("dieselConfigTag" -> x)).toList.filter(x=> ! sc.headers.contains("dieselConfigTag"))
                }.toMap
                ),
              sc.method
            )

            urlx = x.toString
            sc.setUrl(x)

            clog << "Snakking: " + trimmed(sc.toJson, 5000)
            clog << ""
            clog << "Snakking CURL: "
            clog << trimmed(sc.toCurl, 5000)

            eres += EInfo("Snakking " + x.toString, Enc.escapeHtml(trimmed(sc.toCurl))).withPos(pos)

            response = sc.body // make the call

            if(response.length >= Comms.MAX_BUF_SIZE)
              eres += EError(s"BUF_SIZE - read too much from socket (${response.length} + bytes!)", "")

            clog << ("RESPONSE: " + trimmed(response))

            val content = new EEContent(response, sc.iContentType.getOrElse(""), sc.iHeaders.getOrElse(Map.empty), sc.root)
            eres += EInfo("Response", Enc.escapeHtml(trimmed(content.toString, 2000)))
            content
          }

        durationMillis = System.currentTimeMillis() - startMillis
        eres += EDuration(durationMillis)

        // PROCESS the reply

        // does either template have return parm specs?
        val templateSpecs = (templateReq.map(_.parms).getOrElse(Map.empty) ++ templateResp.map(_.parms).getOrElse(Map.empty))
          .filter(_._1 != "content-type")
          .filter(_._1 != "signature")

        // message specified return mappings, if any
        val retSpec = if (in.ret.nonEmpty) in.ret else spec(in).toList.flatMap(_.ret)
        // collect any parm specs
        val specs = retSpec.map(p => (p.name, p.dflt, p.expr.mkString, p.expr))

        val regex = templateSpecs.find(_._1 == "regex").map(_._2).orElse(specs.find(_._1 == "regex").map(_._3))

        // 1. response template specified

        // 2. extract values
        val strs = templateResp.map {tresp =>
            reply.extract(templateSpecs, specs, regex).map(t => new P(t._1, t._2))
          }.getOrElse (
            if(in.entity == "snakk" && in.met == "json") {
              reply.asJsonPayload :: Nil
            } else
              reply.extract(templateSpecs, specs, regex).map(t => new P(t._1, t._2))
          )

        // add the resulting values
        eres += strs.map { x =>
          ctx.put(x)
          EVal(x).withPos(pos)
        }

        // 3. look for stiching dieselTrace
        if (reply.isJson && reply.body.contains("dieselTrace")) {
          val mres = js.parse(response)
          if (mres.contains("dieselTrace")) {
            val trace = DieselJsonFactory.trace(mres("dieselTrace").asInstanceOf[Map[String, Any]])
            eres += trace
          }
        }

        // if no typed result, add a generic text
        if(eres.eres.collect {
          case EVal(p) if p.name == "payload" => p
        }.isEmpty)
          eres += new EVal("payload", reply.body)

        eres.eres ::
          new EVal("snakk.response", reply.body) ::
          Nil

      } getOrElse {

        // no snakk call / template found

        def findin (name:String) =
          in
            .attrs
            .find(_.name == name)
            .orElse(
              spec(in)
                .flatMap(_.attrs.find(_.name == name))
                .map(p=>p.copy(dflt = p.calculatedValue)) // if it's the spec - nobody calculates its value, could be CExpr
            )

        // no template or tcontent => old way without templates
        findin("url").map { u =>
          // is it relative?
          val newurl = if(u.dflt startsWith "http") u.dflt else "http://" +
            ctx.root.asInstanceOf[DomEngECtx].settings.hostport.mkString.mkString + u.dflt

          val sc = new SnakkCall("http", in.arch, newurl, Map.empty, "")
          //          case class SnakkCall (method:String, url:String, headers:Map[String,String], content:String) {
          val ux = url(
            prepUrl(stripQuotes(newurl),
              P("subject", in.entity) ::
              P("verb", in.met) ::
              in.attrs,
              Some(ctx))
            //            in.attrs.filter(_.name != "url").map(p=>(p.name -> p.dflt)).toMap,
            //            stype))
          )

          urlx = ux.toString
          sc.setUrl(ux)

          val content : EEContent = {
            if (sc.method == "open") {
              val response = sc.telnet("localhost", "9000", sc.postContent, Some(eres))
              new EEContent(sc.body, "application/text")
            } else {
              eres += EInfo("Snakking " + urlx, Enc.escapeHtml(trimmed(sc.toJson, 5000))).withPos(pos)
              val response = sc.body
              new EEContent(sc.body, sc.iContentType.getOrElse(""), sc.iHeaders.getOrElse(Map.empty), sc.root)
            }
          }

          durationMillis = System.currentTimeMillis() - startMillis
          eres += EDuration(durationMillis)

          eres += EInfo("Response", html(content.toString))

          // 2. extract values
          val x = if (in.ret.nonEmpty) in.ret else spec(in).toList.flatMap(_.ret)
          val specs = x.map(p => (p.name, p.dflt, p.expr.mkString, p.expr))
          val regex = x.find(_.name == "regex") orElse findin("regex") map (_.dflt)
          val strs = content.extract(Map.empty, specs.filter(_._1 != "regex"), regex)

          // add the resulting values
          eres.eres ::: strs.map(t => new P(t._1, t._2)).map { x =>
            ctx.put(x)
            EVal(x).withPos(pos)
          } ::: new EVal("snakk.response", content.body) ::
          // todo here's where i would add the response headers - make the snakk.response an object?
            new EVal("payload", content.body) ::
            Nil
        } getOrElse
        // need to create a val - otherwise DomApi.rest returns the last Val
          EError("no url attribute for RESTification - and no request template found") ::
          EVal(P("snakk.error", "no url attribute for RESTification - and no request template found")) ::
            Nil
      }
    } catch {
      case t: Throwable => {
        val cause =
        if(t.getCause != null && t.getCause.isInstanceOf[java.net.SocketTimeoutException]) {
          razie.Log.log("error snakking" + t.getMessage)
          t.getCause
        } else {
          razie.Log.log("error snakking", t)
          t
        }

        durationMillis = System.currentTimeMillis() - startMillis
        eres += EDuration(durationMillis)

        eres += //EError("Error snakking: " + urlx, t.toString) ::
          new EError("Exception : ", cause) ::
          EInfo("Response: ", html(response)) ::
            // need to create a val - otherwise DomApi.rest returns the last Val
          EVal(P("snakk.error", html(cause.getMessage, 10000))) ::
            Nil
      }
    }
  }

  def html(s:String, len:Int = 2000) = Enc.escapeHtml(trimmed(s, len))

  override def toString = "$executor::snakk "

  override val messages: List[EMsg] =
    EMsg("snakk", "ffd") ::
    EMsg("snakk", "json") ::
    EMsg("snakk", "text") ::
    EMsg("snakk", "xml") ::
      Nil
}

/** snakk REST APIs */
object EESnakk {

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
    if (u startsWith "/") "http://" +
      ctx.root.asInstanceOf[DomEngECtx].settings.hostport.mkString.mkString +
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

    val C = "url,verb,body,result".split(",")
    var headers = attrs.filter(p=> !(C contains p.name))
    val fbody = attrs.find(_.name == "body")
    var content = f("body")
    content = content

    // figure out content type ?
    if(!attrs.exists(_.name.toLowerCase == "content-type")) {
      if(fbody.exists(_.ttype == WTypes.JSON))
        headers = P("Content-Type", "application/json") :: headers
      else if(fbody.exists(_.ttype == WTypes.XML))
        headers = P("Content-Type", "application/xml") :: headers
    }

    val hattr = headers.map(p=> (p.name, p.calculatedValue)).toSeq.toMap

    new SnakkCall(
      "http",
      verb,
      encurl,
      hattr,
      content
    )
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
  def templateMatchesUrl (t:DTemplate, verb:String, path:String, content:String) = {
    val c = content.replaceFirst("^\n", "") // multiline template start with \n

    val v = c.split(" ").head
    var url = c.split(" ").tail.head.replaceFirst("(?s)\\?.*", "").replaceFirst("(?s)\n.*", "")
    val tpath = c.split("\\?").head

    // remove known prefixes for URL
    if(url.startsWith("/diesel/")) {
      url = url.replaceFirst("/diesel/(mock|rest|wreact/[^/]+/react)/", "")
    }

    // split in segments and match them - each segment to be the same or a variable

    val t = url.split("/")
    val p = path.split("[/.]") // todo I'm doing this because some places rely on react/class/action works as well as react/class.action

    val zipped = t.zip(p)
    val res = t.size == p.size && zipped.size == t.size && zipped.foldLeft(true)((x,y) => x && (y._1 == y._2 || y._1.startsWith("$")))

    // maybe use a better matches below...
//    a.zip(b).foldLeft(true)((a, b) => a && b._1 == b._2 || b._2.matches("""\$\{([^\}]*)\}"""))

    res
  }

  /** prepare the template content - expand $parm expressions */
  def prepStr2(content: String, attrs: Attrs)(implicit ctx:ECtx) = {
    // what style is used?
    if(content contains "${") {
      val PAT = """\$\{([^\}]*)\}""".r
      val s1 = PAT.replaceAllIn(content, { m =>
        (new SimpleExprParser).parseExpr(m.group(1)).map {e=>
          P ("x", "", "", "", "", Some(e)).calculatedValue
        } getOrElse
          s"{ERROR: ${m.group(1)}"
      })
      s1
    } else
      prepStr(content, attrs, Some(ctx))
  }

  /** prepare the template content - expand $parm expressions */
  def expandExpr(url: String, attrs: Option[Attrs], ctx:Option[ECtx]):String = {
    val PATTERN = """\$\{([^\}]*)\}""".r
    //    val eeEscaped = es.replaceAllLiterally("(", """\(""").replaceAllLiterally(")", """\)""")

    var u = ""
    try {
      u = PATTERN.replaceSomeIn(url, { m =>
        val n = m.group(1)
        attrs.flatMap(_.find(_.name == n)).orElse(ctx.flatMap(_.getp(n))).map(x =>
          ctx.map { implicit ctx =>
            stripQuotes(x.calculatedValue)
          }.getOrElse {
            stripQuotes(x.dflt)
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
          stripQuotes(x.dflt)
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

    val PAT = """\$\{([^\}]*)\}""".r
    s1 = PAT.replaceSomeIn(s1, { m =>
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
      s.dflt.replaceAll("\\[\\]", "")
    }

    val output = in.attrs.find(_.name == "result").map(_.dflt)

    // should parse just one in put parm, but we can parse many and aggregate the results
    in.attrs.filter(p=> p.name != "schema" && p.name != "result").headOption.toList.flatMap {input=>
      val parsed = new FFDPayload(input.dflt, schema.mkString).parse

      val results = parsed.getResult.map {values=>
        val x = new mutable.HashMap[String,Any]
        values.map{ p=>
          x.put(p.name, p.dflt)
        }

        razie.js.tojsons(x.toMap)
      }

      results.toList.flatMap {res=>
        output.toList.map(name=>P(name, res, WTypes.JSON)) :::
          EVal(P("payload", res, WTypes.JSON)) :: Nil
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
      s.dflt.replaceAll("\\[\\]", "")
    }

    val output = in.attrs.find(_.name == "result").map(_.dflt)

    val parms = in.attrs.filter(p=> p.name != "schema" && p.name != "result")
    val results = new FFDPayload("", schema.mkString).build(ctx)

    results.getResult.toList.flatMap {res=>
      output.toList.map(name=>P(name, res)) :::
        EVal(P("payload", res)) :: Nil
    } ::: results.getErrors.map(EError(_))
  }

  override def toString = "$executor::snakk "
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
