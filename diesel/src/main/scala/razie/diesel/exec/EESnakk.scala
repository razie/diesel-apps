/**
  * ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package mod.diesel.model.exec

import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import java.net.{Socket, URI}

import com.novus.salat._
import com.razie.pub.comms.Comms
import razie.db.RazSalatContext._
import razie.diesel.dom.RDOM._
import razie.diesel.dom.{DTemplate, _}
import razie.diesel.engine.RDExt.{DieselJsonFactory, EEContent, spec}
import razie.diesel.engine.{DomEngECtx, InfoAccumulator}
import razie.diesel.ext.{MatchCollector, _}
import razie.wiki.Enc
import razie.{Snakk, SnakkUrl, js}

import scala.Option.option2Iterable
import scala.collection.mutable.ListBuffer

/** a single snakk call to make
  *
  * @param protocol http, telnet
  **/
case class SnakCall(protocol: String, method: String, url: String, headers: Map[String, String], content: String) {
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
    iContentType = Some(conn.getHeaderField("Content-Type"))
    ibody = Some(Comms.readStream(is))
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

}

/** snakk REST APIs */
class EESnakk extends EExecutor("snakk") {
  import EESnakk._

  /** can I execute this task? */
  override def test(m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) = {
    def known (s:String) = (s contains "GET") || (s contains "POST") || (s contains "TELNET") || (s contains "HTTP")

    known(m.stype) ||
      spec(m).exists(m => known(m.stype) ||
        ctx.findTemplate(m.entity + "." + m.met).isDefined)
  }

  /** execute the task then */
  override def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] = {
    val templateReq  = ctx.findTemplate(in.entity + "." + in.met, "req")
    val templateResp = ctx.findTemplate(in.entity + "." + in.met, "resp")

    // expanded template content
    val formattedTemplate = templateReq.map(_.content).map { content =>
      // todo either this or prepUrl not both
      val PAT = """\$\{([^\}]*)\}""".r
      val s1 = PAT.replaceAllIn(content, { m =>
        ctx(m.group(1))
      })
      s1
    }

    import razie.Snakk._

    var eres = new InfoAccumulator()
    var response = "" // actual textual response
    var urlx = "?" // url, filled later for error rep

    try {

      // 1. prepare the request
      formattedTemplate.map(parseTemplate(templateReq, _, in.attrs)).map { sc =>
        val newurl = if (sc.url startsWith "/") "http://" + ctx.hostname.mkString + sc.url else sc.url
        // with templates

        val content =
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
                  P("verb", in.met) :: in.attrs),
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

            eres += EInfo("Snakking " + x.url, Enc.escapeHtml(sc.toJson)).withPos(Some(templateReq.get.pos))
            response = sc.body // make the call
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
        val specs = retSpec.map(p => (p.name, p.dflt, p.expr.mkString))

        val regex = temp.find(_._1 == "regex").map(_._2).orElse(specs.find(_._1 == "regex").map(_._3))


        // 1. response template specified
        if()

        // 2. extract values
        val strs = templateResp.map {tresp =>
          content.extract(temp, specs, regex)
          }
          .getOrElse(content.extract(temp, specs, regex))

        val pos = (templateReq.map(_.pos)).orElse(spec(in).flatMap(_.pos))

        // add the resulting values
        eres += strs.map(t => new P(t._1, t._2)).map { x =>
          ctx.put(x)
          EVal(x).withPos(pos)
        }

        // 3. look for stiching dieselTrace
        if (content.isJson) {
          val mres = js.parse(response)
          if (mres.contains("dieselTrace")) {
            val trace = DieselJsonFactory.trace(mres("dieselTrace").asInstanceOf[Map[String, Any]])
            eres += trace
          }
        }

        eres.eres

      } getOrElse {

        // no template or tcontent => old way without templates
        in.attrs.find(_.name == "url").orElse(
          spec(in).flatMap(_.attrs.find(_.name == "url"))
        ).map { u =>
          val sc = new SnakCall("http", in.arch, u.dflt, Map.empty, "")
          //          case class SnakCall (method:String, url:String, headers:Map[String,String], content:String) {
          val ux = url(
            prepUrl(stripQuotes(u.dflt),
              P("subject", in.entity) ::
                P("verb", in.met) :: in.attrs)
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
          val specs = x.map(p => (p.name, p.dflt, p.expr.mkString))
          val regex = specs.find(_._1 == "regex") map (_._2)
          val strs = content.extract(Map.empty, specs, regex)

          val pos = (templateReq.map(_.pos)).orElse(spec(in).flatMap(_.pos))
          // add the resulting values
          strs.map(t => new P(t._1, t._2)).map { x =>
            ctx.put(x)
            EVal(x).withPos(pos)
          }
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
  def parseTemplate(t: Option[DTemplate], is: String, attrs: Attrs) : SnakCall = {
    if (t.flatMap(_.parms.get("protocol")).exists(_ == "telnet"))
      parseTelnetTemplate(t, is, attrs)
    else parseHttpTemplate(t, is, attrs)
  }

  def parseTelnetTemplate(t: Option[DTemplate], is: String, attrs: Attrs) : SnakCall = {
    // templates start with a \n
    val s = if (is startsWith "\n") is.substring(1) else is
    val lines = s.lines
    val REX = """(\w+) ([.\w]+)[:/ ](\w+).*""".r
    val REX(verb, host, port) = lines.take(1).next
    val content = lines.mkString("\n")
    new SnakCall("telnet", "telnet", s"$host:$port", Map.empty, prepStr(content, attrs).replaceFirst("(?s)\n\r?$", ""))
  }

  def parseHttpTemplate(template: Option[DTemplate], is: String, attrs: Attrs) : SnakCall = {
    // templates start with a \n
    val s = if (is startsWith "\n") is.substring(1) else is
    val verb = s.split(" ").head
    val url = s.split(" ").tail.head.replaceFirst("\n.*", "")
    val headers = if (s matches "(?s).*\n\r?\n\r?.*") s.replaceFirst("(?s)\n\r?\n\r?.*", "").lines.drop(1).mkString("\n") else ""
    var content = if (s matches "(?s).*\n\r?\n\r?.*") s.replaceFirst("(?s).*\n\r?\n\r?", "") else ""
    if (content endsWith ("\n")) content = content.substring(0, content.length - 1)
    val hattr = prepStr(headers, attrs).lines.map(_.split(":", 2)).collect {
      case a if a.size == 2 => (a(0).trim, a(1).trim)
    }.toSeq.toMap

    new SnakCall("http", verb, url, hattr, prepStr(content, attrs).replaceFirst("(?s)\n\r?$", ""))
  }

  /** prepare the template content - expand $parm expressions */
  private def prepStr(url: String, attrs: Attrs) = {
    //todo add expressions like ${...}
    val PATT = """(\$\w+)*""".r
    val u = PATT.replaceSomeIn(url, { m =>
      val n = if (m.matched.length > 0) m.matched.substring(1) else ""
      attrs.find(_.name == n).map(x =>
        stripQuotes(x.dflt)
      )
    })
    u
  }

  /** prepare the URL - expand $parm expressions */
  private def prepUrl(url: String, attrs: Attrs) = {
    //todo add expressions like ${...}
    val PATTERN = """(\$\w+)*""".r
    val u = PATTERN.replaceSomeIn(url, { m =>
      val n = if (m.matched.length > 0) m.matched.substring(1) else ""
      attrs.find(_.name == n).map(x =>
        Enc.toUrl(stripQuotes(x.dflt))
      )
    })
    val res = new URI(u).toString()
    res
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
  def toHtml = EMsg("", e, a, Nil).withPos(pos).toHtmlInPage
}


