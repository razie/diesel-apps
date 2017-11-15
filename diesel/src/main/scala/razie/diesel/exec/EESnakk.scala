/**
  * ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package mod.diesel.model.exec

import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import java.net.{Socket, URI}
import java.util.regex.Pattern

import com.novus.salat._
import com.razie.pub.comms.Comms
import razie.db.RazSalatContext._
import razie.diesel.dom.RDOM._
import razie.diesel.dom._
import razie.diesel.engine.RDExt.{DieselJsonFactory, EEContent, spec}
import razie.diesel.engine.{DomEngECtx, InfoAccumulator}
import razie.diesel.ext.{MatchCollector, _}
import razie.diesel.snakk.FFDPayload
import razie.tconf.DTemplate
import razie.wiki.Enc
import razie.{Snakk, SnakkUrl, clog, js}

import scala.Option.option2Iterable
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try
import scala.util.parsing.json.JSONObject
import scala.xml.{Elem, Node}

/** a single snakk call to make
  *
  * @param protocol http, telnet
  **/
case class SnakCall(protocol: String, method: String, url: String, headers: Map[String, String], content: String, template:Option[DTemplate] = None) {
  def toJson = grater[SnakCall].toPrettyJSON(this)

  var isurl: SnakkUrl = null

  def setUrl(u: SnakkUrl) = {
    isurl = u
  }

  def postContent = if (content != null && content.length > 0) Some(content) else None

  var ibody: Option[String] = None
  var iContentType: Option[String] = None

  def body = ibody.getOrElse {
    val conn = Snakk.conn(isurl, postContent)
    val is = conn.getInputStream()
    ibody = Some(Comms.readStream(is))

    iContentType = conn.getHeaderField("Content-Type") match {
      case s if s != null && s.length > 0 => Some(s.toLowerCase)
      case null => {
        // try to determine it
        if(ibody.exists(_.startsWith("<?xml"))) Some("application/xml")
        else if(ibody.exists(_.startsWith("{"))) Some("application/json")
        else None
      }
    }

    ibody.get
  }

  /** xp root for either xml or json body */
  def root: Option[Snakk.Wrapper[_]] = {
    val b = body
    iContentType match {
      case Some("application/xml") => Some(Snakk.xml(b))
      case Some("application/json") => Some(Snakk.json(b))
      case x@_ => {
        //        throw new IllegalStateException ("unknown content-type: "+x)
        None
      }
    }
  }

  def telnet(hostname: String, port: String, send: Option[String], info: Option[InfoAccumulator]): String = {
    var res = ""
    try {
      val pingSocket = new Socket(hostname, port.toInt);
      pingSocket.setSoTimeout(3000) // 1 sec
      val out = new PrintWriter(pingSocket.getOutputStream(), true);
      val ins = pingSocket.getInputStream();

      //      out println "GET / HTTP/1.1"
      //      out println "GET /diesel/test/plus/a/b HTTP/1.1"
      //      out println ""
      send.toList.flatMap(_.lines.toList) map { outs =>
        out.println(outs)
        info map (_ += EInfo("telnet Sent line: " + outs))
      }

      val inp = new BufferedReader(new InputStreamReader(pingSocket.getInputStream()));
      //      for(x <- inp.lines.iterator().asScala)
      //        res = res + x
      //      val res = inp.readLine()

      //      res = inp.readLine()
      //      res = res + Comms.readStream(ins)

//      try {
        val buff = new Array[Byte](1024);
        var ret_read = 0;

        do {
          ret_read = ins.read(buff);
          if (ret_read > 0) {
            res = res + new String(buff, 0, ret_read)
            //          System.out.print(new String(buff, 0, ret_read));
          }
        }
        while (ret_read >= 0);
//      }

      ibody = Some(res)
      iContentType = Some("application/text")

      out.close();
      ins.close();
      pingSocket.close();

      res
    } catch {
      case e: Throwable => {
        info map (_ += EError("telnet Exception:" + e.toString))
        ibody = Some(res)
        return res;
      }
    }
  }

  def isXml (ct:Option[String]) : Boolean =
    ct.map(_.toLowerCase).exists(s=> s=="application/xml" || s=="text/xml")

  /** use this call/template to parse incoming message (either request to us or a reply) */
  def parseIncoming (incoming:String, inSpecs : List[P], incomingMetas: NVP) : NVP = {
    val x = (
      isXml(
        template.flatMap(
          _.parms.find(t=> t._1.toLowerCase == "content-type").map(_._2)
        )
      )
    )

    if(x)
        parseIncomingXml(incoming, inSpecs, incomingMetas)
    else
      parseIncomingMatch(incoming, inSpecs, incomingMetas)
  }

  /** use this call/template to parse incoming message (either request to us or a reply) */
  def parseIncomingXml (incoming:String, inSpecs : List[P], incomingMetas: NVP) : NVP = {
    // parse the template and remember DOM so we can get the XPATH later
    val tx = Snakk.xml(this.content)
    val n = tx.node

    // todo find all $name expressions instead of having to
    def findExprFor (node:Node, name:String, path:String="/") : Option[String] = {
//      clog << "XML "+node.getClass.getName + "-"+ node.label + "-"+node.text
      // find the xpath for value $name
        // is this it?
      if(node.text == "$"+name) Some(path)
      else {
        (node.child collect  {
          case t:scala.xml.Text => None //findExprFor(t, name, path+"/"+t.label)
          case t:scala.xml.Elem => findExprFor(t, name, path+"/"+t.label)
          case t:scala.xml.Node => findExprFor(t, name, path+"/"+t.label)
        }).find(_.isDefined).map(_.get)
      }
    }

    def forceAttr (path:String) = path.replaceFirst("(.*)/([^/]*)$", "$1/@$2")

    val xinSpecs = inSpecs.map(p => (p.name, p.dflt, p.expr.mkString))

    // parse incoming as xml
    val ix = Snakk.xml(incoming)

    // find and make message with parms
    var parms =
      (
        for (g <- xinSpecs)
          yield (
            g._1, {
            val e = findExprFor(tx.node, g._1, "/"+tx.node.label)
            val v = e.map{e =>
//              clog << "XP "+e
              ix \@@ forceAttr(e)
            }
            v.getOrElse(g._2)
          }
          )
        ).filter(_._2 != null).toMap

    parms
  }

  /** parse incoming message with pattern matching */
  def parseIncomingMatch (incoming:String, inSpecs : List[P], incomingMetas: NVP) : NVP = {
    // turn template into regex
    var re = this.content.replaceAll("""\$\{(.+)\}""", "$1")
    re = re.replaceAll("""\$(\w+)""", "(?<$1>.*)?")
    re = re.replaceAll("""[\r\n ]""", """\\s*""")
    re = "(?sm)" + re + ".*"

    val xinSpecs = inSpecs.map(p => (p.name, p.dflt, p.expr.mkString))

    // find and make message with parms
    val jrex = Pattern.compile(re).matcher(incoming)
    val hasit = jrex.find()
    var parms = if (hasit)
      (
        for (g <- xinSpecs)
          yield (
            g._1,
            Try {
              jrex.group(g._1)
            }.getOrElse(g._2)
          )
        ).filter(_._2 != null).toMap
    else Map.empty[String,String]

    parms
  }
}

/** snakk REST APIs */
class EESnakk extends EExecutor("snakk") {
  import EESnakk._

  /** can I execute this task? */
  override def test(m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) = {
    def known (s:String) = (s contains "GET") || (s contains "POST") || (s contains "TELNET") || (s contains "HTTP")

    m.entity == "snakk" && m.met == "ffd" ||
    m.entity == "snakk" && m.met == "ffdFormat" ||
    known(m.stype) ||
      spec(m).exists(m => known(m.stype) ||
        ctx.findTemplate(m.entity + "." + m.met).isDefined)
  }

  /** execute the task then */
  override def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] = {
    if(in.entity == "snakk" && in.met == "ffd")
      return snakkFfd(in, destSpec)
    else if(in.entity == "snakk" && in.met == "ffdFormat")
      return formatFfd(in, destSpec)

    val templateReq  = ctx.findTemplate(in.entity + "." + in.met, "request")
    val templateResp = ctx.findTemplate(in.entity + "." + in.met, "response")

    // expanded template content
    val formattedTemplate = templateReq.map(_.content).map { content =>
      // todo either this or prepUrl not both

      // what style is used?
      if(content contains "${") {
        val PAT = """\$\{([^\}]*)\}""".r
        val s1 = PAT.replaceAllIn(content, { m =>
          ctx(m.group(1))
        })
        s1
      } else
        prepStr(content, in.attrs, Some(ctx))
    }

    import razie.Snakk._

    var eres = new InfoAccumulator()
    var response = "" // actual textual response
    var urlx = "?" // url, filled later for error rep

    try {

      // 1. prepare the request and make the call
      formattedTemplate.map(parseTemplate(templateReq, _, in.attrs, Some(ctx))).map { sc =>
        val newurl = if (sc.url startsWith "/") "http://" +
          ctx.root.asInstanceOf[DomEngECtx].settings.hostport.mkString.mkString +
          sc.url
        else sc.url

        // with templates

        val reply =
          if (templateReq.flatMap(_.parms.get("protocol")).exists(_ == "telnet")) {
            // telnet
            eres += EInfo("Snakking TELNET ", Enc.escapeHtml(sc.toJson)).withPos(Some(templateReq.get.pos))
            //            response = sc.body // make the call
            val REX =
              """([.\w]+)[:/ ](\w+).*""".r
            val REX(host, port) = newurl
            response = sc.telnet(host, port, sc.postContent, Some(eres))
            eres += EInfo("Response", Enc.escapeHtml(response))
            val content = new EEContent(response, sc.iContentType.getOrElse(""), None)
            content
          } else {
            // http
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
                    ("dieselNodeId" -> node.id)).toList ++
                    ctx.root.asInstanceOf[DomEngECtx].settings.userId.map(x =>
                      ("dieselUserId" -> x)).toList ++
                    ctx.root.asInstanceOf[DomEngECtx].settings.configTag.map(x =>
                      ("dieselConfigTag" -> x)).toList
                }.toMap
                ),
              sc.method
            )

            urlx = x.toString
            sc.setUrl(x)

            clog << "Snakking: " + sc.toJson

            eres += EInfo("Snakking " + x.url, Enc.escapeHtml(sc.toJson)).withPos(Some(templateReq.get.pos))
            response = sc.body // make the call
            clog << ("RESPONSE: \n" + response)
            eres += EInfo("Response", Enc.escapeHtml(response))
            val content = new EEContent(response, sc.iContentType.getOrElse(""), sc.root)
            content
          }

        // PROCESS the reply

        // does either template have return parm specs?
        val temp = (templateReq.get.parms ++ templateResp.map(_.parms).getOrElse(Map.empty))
          .filter(_._1 != "content-type")

        // message specified return mappings, if any
        val retSpec = if (in.ret.nonEmpty) in.ret else spec(in).toList.flatMap(_.ret)
        // collect any parm specs
        val specs = retSpec.map(p => (p.name, p.dflt, p.expr.mkString, p.expr))

        val regex = temp.find(_._1 == "regex").map(_._2).orElse(specs.find(_._1 == "regex").map(_._3))


        // 1. response template specified

        // 2. extract values
        val strs = templateResp.map {tresp =>
            reply.extract(temp, specs, regex)
          }.getOrElse (
            reply.extract(temp, specs, regex)
          )

        val pos = (templateReq.map(_.pos)).orElse(spec(in).flatMap(_.pos))

        // add the resulting values
        eres += strs.map(t => new P(t._1, t._2)).map { x =>
          ctx.put(x)
          EVal(x).withPos(pos)
        }

        // 3. look for stiching dieselTrace
        if (reply.isJson) {
          val mres = js.parse(response)
          if (mres.contains("dieselTrace")) {
            val trace = DieselJsonFactory.trace(mres("dieselTrace").asInstanceOf[Map[String, Any]])
            eres += trace
          }
        }

        eres.eres :: new EVal("snakk.response", reply.body) :: new EVal("result", reply.body) :: Nil

      } getOrElse {

        def findin (name:String) =
          in
            .attrs
            .find(_.name == name)
            .orElse(
              spec(in)
                .flatMap(_.attrs.find(_.name == name))
                .map(p=>p.copy(dflt = p.calculateValue)) // if it's the spec - nobody calculates its value, could be CExpr
            )

        // no template or tcontent => old way without templates
        findin("url").map { u =>
          // is it relative?
          val newurl = if(u.dflt startsWith "http") u.dflt else "http://" +
            ctx.root.asInstanceOf[DomEngECtx].settings.hostport.mkString.mkString + u.dflt

          val sc = new SnakCall("http", in.arch, newurl, Map.empty, "")
          //          case class SnakCall (method:String, url:String, headers:Map[String,String], content:String) {
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

          eres += EInfo("Snakking " + ux.url, Enc.escapeHtml(sc.toJson)).withPos(spec(in).flatMap(_.pos))

          val content = {
            if (sc.method == "open") {
              val response = sc.telnet("localhost", "9000", sc.postContent, Some(eres))
              new EEContent(sc.body, "application/text", None)
            } else {
              val response = sc.body
              eres += EInfo("Response", Enc.escapeHtml(response))

              new EEContent(sc.body, sc.iContentType.getOrElse(""), sc.root)
            }
          }

          // 2. extract values
          val specxxx = spec(in)
          val x = if (in.ret.nonEmpty) in.ret else spec(in).toList.flatMap(_.ret)
          val specs = x.map(p => (p.name, p.dflt, p.expr.mkString, p.expr))
          val regex = x.find(_.name == "regex") orElse findin("regex") map (_.dflt)
          val strs = content.extract(Map.empty, specs.filter(_._1 != "regex"), regex)

          val pos = (templateReq.map(_.pos)).orElse(spec(in).flatMap(_.pos))

          // add the resulting values
          eres.eres ::: strs.map(t => new P(t._1, t._2)).map { x =>
            ctx.put(x)
            EVal(x).withPos(pos)
          } ::: new EVal("snakk.response", content.body) :: new EVal("result", content.body) :: Nil
        } getOrElse
          EError("no url attribute for RESTification ") :: Nil
      }
    } catch {
      case t: Throwable => {
        razie.Log.log("error snakking", t)
        eres += EError("Error snakking: " + urlx, t.toString) ::
          EError("Exception : " + t.toString) ::
          EError("Response: ", Enc.escapeHtml(response)) :: Nil
      }
    }
  }

  override def toString = "$executor::snakk "
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
  def parseTemplate(t: Option[DTemplate], is: String, attrs: Attrs, ctx:Option[ECtx]=None) : SnakCall = {
    if (t.flatMap(_.parms.get("protocol")).exists(_ == "telnet"))
      parseTelnetTemplate(t, is, attrs,ctx)
    else parseHttpTemplate(t, is, attrs,ctx)
  }

  def parseTelnetTemplate(t: Option[DTemplate], is: String, attrs: Attrs, ctx:Option[ECtx]) : SnakCall = {
    // templates start with a \n
    var xis = is.replaceAll("\r", "")
    val s = if (xis startsWith "\n") xis.substring(1) else is
    val lines = s.lines
    val REX = """(\w+) ([.\w]+)[:/ ](\w+).*""".r
    val REX(verb, host, port) = lines.take(1).next
    val content = lines.mkString("\n")
    new SnakCall("telnet", "telnet", s"$host:$port", Map.empty, prepStr(content, attrs, ctx).replaceFirst("(?s)\n\r?$", ""))
  }

  def parseHttpTemplate(template: Option[DTemplate], is: String, attrs: Attrs, ctx:Option[ECtx]) : SnakCall = {
    // templates start with a \n
    var xis = is.replaceAll("\r", "")
    val s = if (xis startsWith "\n") xis.substring(1) else xis
    val verb = s.split(" ").head
    val url = prepUrl(s.split(" ").tail.head.replaceFirst("\n.*", ""), attrs, ctx)
    val headers = if (s matches "(?s).*\n\r?\n\r?.*") s.replaceFirst("(?s)\n\r?\n\r?.*", "").lines.drop(1).mkString("\n") else ""
    var content = if (s matches "(?s).*\n\r?\n\r?.*") s.replaceFirst("(?s).*\n\r?\n\r?", "") else ""
    if (content endsWith ("\n")) content = content.substring(0, content.length - 1)
    val hattr = prepStr(headers, attrs, ctx).lines.map(_.split(":", 2)).collect {
      case a if a.size == 2 => (a(0).trim, a(1).trim)
    }.toSeq.toMap

    new SnakCall(
      "http",
      verb,
      url,
      hattr,
      prepStr(content, attrs, ctx).replaceFirst("(?s)\n\r?$", ""),
      template
    )
  }

  /** prepare the template content - expand $parm expressions */
  private def prepStr(url: String, attrs: Attrs, ctx:Option[ECtx]) = {
    //todo add expressions like ${...}
    val PATT = """(\$\w+)*""".r
    val u = PATT.replaceSomeIn(url, { m =>
      val n = if (m.matched.length > 0) m.matched.substring(1) else ""
      attrs.find(_.name == n).orElse(ctx.flatMap(_.getp(n))).map(x =>
        stripQuotes(x.dflt)
      )
    })
    u
  }

  /** prepare the URL - expand $parm expressions */
  def prepUrl(url: String, attrs: Attrs, ctx:Option[ECtx]) : String = {
    //todo add expressions like ${...}
    val PATTERN = """(\$\w+)*""".r
    val u = PATTERN.replaceSomeIn(url, { m =>
      val n = if (m.matched.length > 0) m.matched.substring(1) else ""
      attrs.find(_.name == n).orElse(ctx.flatMap(_.getp(n))).map(x =>
        // todo should encurl whatever is after ? so the :// and : stay the same
//        Enc.toUrl(stripQuotes(x.dflt))
        stripQuotes(x.dflt)
      )
    })
    val res = new URI(u).toString()
    res
  }

  def formatTemplate (content:String, ctx:ECtx) = {
    // todo either this or prepUrl not both
    val PATTERN = """(\$\w+)*""".r
    var s1 = PATTERN.replaceSomeIn(content, { m =>
      val n = if (m.matched.length > 0) m.matched.substring(1) else ""
      if(n.length > 1)
        ctx.get(n) orElse Some("") // orElse causes $msg to expand to nothing if no msg
      else None
//        stripQuotes(x.dflt)
    })
    val PAT = """\$\{([^\}]*)\}""".r
    s1 = PAT.replaceSomeIn(s1, { m =>
      val n = m.group(1)
      if(n.length > 1) ctx.get(n) else None
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
          EVal(P("result", res, WTypes.JSON)) :: Nil
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
        EVal(P("result", res)) :: Nil
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


