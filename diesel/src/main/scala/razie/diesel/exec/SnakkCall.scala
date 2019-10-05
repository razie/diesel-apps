/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package razie.diesel.exec

import com.novus.salat._
import com.razie.pub.comms.Comms
import java.io.PrintWriter
import java.net.Socket
import java.util.regex.Pattern
import org.json.JSONObject
import razie.db.RazSalatContext._
import razie.diesel.dom.RDOM._
import razie.diesel.engine.{EContent, InfoAccumulator}
import razie.diesel.ext._
import razie.tconf.DTemplate
import razie.{Snakk, SnakkRequest, SnakkResponse, SnakkUrl}
import scala.Option.option2Iterable
import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.util.Try
import scala.xml.Node

/** a single snakk call to make - goes beyond Snakk.body
  *
  * @param protocol http, telnet
  * @param method GET, POST, open etc
  */
case class SnakkCall(protocol: String, method: String, url: String, headers: Map[String, String], content: String, template:Option[DTemplate] = None) {
  def toJson = grater[SnakkCall].toPrettyJSON(this)

  def toSnakkRequest (id:String="") = SnakkRequest(protocol, method, url, headers, content, id)

  // todo use the isurl, not the parameters passed in?
  def toCurl = {
    "curl -k " +
      ("-X " + method) +
      (headers.map{t=>
        s""" -H '${t._1}:${t._2}' """
      }).mkString(" ") +
      (
        if(content != "") s" -d '$content' " else " "
      ) +
    s"'${isurl.url.toString}'"
  }

  var pro : Option[Promise[SnakkResponse]] = None

  // todo this class won't work if this is not set from the outside - that's wrong and confusing, to say the least...
  private var isurl: SnakkUrl = null

  def setUrl(u: SnakkUrl) = {
    isurl = u
    this
  }

  def postContent = if (content != null && content.length > 0) Some(content) else None

  /** response body */
  var ibody: Option[String] = None
  /** response content type */
  var iContentType: Option[String] = None
  /** response headers */
  var iHeaders: Option[Map[String, String]] = None
  /** response code */
  var icode: Option[Int] = None

  def body = ibody getOrElse makeCall

  /** make the call and return all details */
  def eContent = {
    new EContent(body, this.iContentType.getOrElse(""), this.icode.getOrElse(-1), this.iHeaders.getOrElse(Map.empty), this.root)
  }

  def makeCall = {
    val conn = Snakk.conn(isurl, postContent)
    val is = conn.getInputStream()
    ibody = Some(Comms.readStream(is))

    // process result
    iContentType = conn.getHeaderField("Content-Type") match {
      case s if s != null && s.length > 0 => Some(s.toLowerCase)
      case "" | null => {
        // try to determine it
        if(ibody.exists(_.startsWith("<?xml"))) Some("application/xml")
        else if(ibody.exists(_.startsWith("{"))) Some("application/json")
        else None
      }
    }

    val x = conn.getHeaderFields.keySet().toArray.toList
    iHeaders = Some(x.filter(_ != null).map(x => (x.toString, conn.getHeaderField(x.toString))).toMap)
    icode = Some(Comms.getResponseCode(conn))

    ibody.get
  }

  def future : Future[SnakkResponse] = {
    if(! this.pro.isDefined) {
      this.pro = Some(Promise[SnakkResponse]())
      SnakkCallAsyncList.put(this)
    }
    pro.get.future
  }

  /** xp root for either xml or json body */
  def root: Option[Snakk.Wrapper[_]] = {
    val b = body
    iContentType match {
      case Some(s) if s.trim.startsWith("application/xml") => Some(Snakk.xml(b))
      case Some(s) if s.trim.startsWith("application/json") => Some(Snakk.json(b))
      case x@_ => {
        //        throw new IllegalStateException ("unknown content-type: "+x)
        None
      }
    }
  }

  /** snakk content from a telnet connection
    *
    * @param hostname
    * @param port
    * @param send commands to send (separated by \n)
    * @param info
    * @return
    */
  def telnet(hostname: String, port: String, send: Option[String], info: Option[InfoAccumulator]): String = {
    var pingSocket : Socket = null

    var res = ""
    try {
//      pingSocket = new Socket("www.google.ca", 80);
      pingSocket = new Socket(hostname, port.toInt);
      pingSocket.setSoTimeout(5000) // 5 sec
      val out = new PrintWriter(pingSocket.getOutputStream(), true);
      val ins = pingSocket.getInputStream();

//            out println "GET / HTTP/1.1"
      //      out println "GET /diesel/test/plus/a/b HTTP/1.1"
//      out println ""

      // todo this should be done in CExp
      send.map { s=>
        // allow \n
        s
          .replaceAll("\\\\n", "\n")
          .replaceAll("\\\\r", "\r")
      }.toList.flatMap(_.lines.toList) map { outs =>
        out.println(outs)
        info map (_ += EInfo("telnet Sent line: " + outs))
      }

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
      iContentType = Some("application/text") // assume telnet returns text

      out.close();
      ins.close();
      pingSocket.close();

      res
    } catch {
      // for timeouts, don't print stack traces, it's confusing
      case e: java.net.SocketTimeoutException => {
        razie.Log.log("snakk.telnet Exception" + e.getMessage)
        info map (_ += new EWarning("telnet Exception:", e.getMessage))
        ibody = Some(res)
        pingSocket.close();
        return res;
      }
      case e: Throwable => {
        razie.Log.log("snakk.telnet Exception", e)
        info map (_ += new EWarning("telnet Exception:", e))
        ibody = Some(res)
        pingSocket.close();
        return res;
      }
    }
  }

  def isJson (ct:Option[String]) : Boolean =
    ct.map(_.toLowerCase).exists(s=> s=="application/json" || s=="text/json")

  def isXml (ct:Option[String]) : Boolean =
    ct.map(_.toLowerCase).exists(s=> s=="application/xml" || s=="text/xml")

  /** use this call/template to parse incoming message (either request to us or a reply).
    *
    * in this case, this.content is the template
    */
  def parseIncoming (incoming:String, inSpecs : List[P], incomingMetas: NVP) : NVP = {
    val ct = template.flatMap(
      _.parms.find(t=> t._1.toLowerCase == "content-type").map(_._2)
    )

    if( isXml(ct) )
      parseIncomingXml(incoming, inSpecs, incomingMetas)
    else if( isJson(ct) )
      parseIncomingJson(incoming, inSpecs, incomingMetas)
    else
      parseIncomingMatch(incoming, inSpecs, incomingMetas)
  }

  /** use this call/template to parse incoming message (either request to us or a reply) */
  def parseIncomingXml (incoming:String, inSpecs : List[P], incomingMetas: NVP) : NVP = {
    // parse the template and remember DOM so we can get the XPATH later
    val tx = Snakk.xml(this.content)
    val n = tx.node

    /** find where in the template this name is and return the xpath to it's location */
    // todo find all $name expressions instead of having to
    def findExprFor (node:Node, name:String, path:String="/") : Option[String] = {
//      clog << "XML "+node.getClass.getName + "-"+ node.label + "-"+node.text
      // find the xpath for value $name
        // is this it?
      if(node.text == s"$${$name}") Some(path)
      else {
        (node.child collect  {
          case t:scala.xml.Text => None //findExprFor(t, name, path+"/"+t.label)
          case t:scala.xml.Elem => findExprFor(t, name, path+"/"+t.label)
          case t:scala.xml.Node => findExprFor(t, name, path+"/"+t.label)
        }).find(_.isDefined).map(_.get)
      }
    }

    def forceAttr (path:String) = path.replaceFirst("(.*)/([^/]*)$", "$1/@$2")

    // inSpecs PLUS any parms defined in the template
    val out = findTemplateParms(this.content)
    val xinSpecs = inSpecs.map(p => (p.name, p.currentStringValue, p.expr.mkString)) ::: out.filter(xx=> !inSpecs.exists(_.name == xx)).map(p => (p, "", ""))

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
            // if there was no default, do not use this parm - it wasn't found anyways
            // this allows us later to use "x not defined" - otherwise, it would be defined but unknown
            // worst case scenario, return an WTypes.UNDEFINED, but not an empty
            // NOTE that the filter also removes empty values found by \@@
            v.filter(_ != "").getOrElse(if(g._2 != "") g._2 else null)
          }
          )
        ).filter(_._2 != null).toMap

    parms
  }

  /** parse a templetized url
    *
    * @param turl template
    * @param url incoming
    */
  def parseUrl(turl:String, url:String, inSpecs:List[P], incomingMetas:NVP) = {
    val path = turl.replaceFirst("(?s)\\?.*", "") // cut the end

    parseIncomingTemplate (path, url, inSpecs, incomingMetas, true)
  }

  /** use this call/template to parse incoming message (either request to us or a reply) */
  def parseIncomingJson (incoming:String, inSpecs : List[P], incomingMetas: NVP) : NVP = {
    // parse the template and remember DOM so we can get the XPATH later
    // this.content is processed, find the original template content
    val tc = template.map(_.content).map{s=>
      if(s matches "(?s).*\n\r?\n\r?.*") s.replaceFirst("(?s).*\n\r?\n\r?", "") else ""
    }.getOrElse(this.content)

    val js1 = new JSONObject(tc)
    import scala.collection.JavaConverters._

    // todo just one level deep for now - a flat JSON
    // get the values that start with a $
    val out = js1.keys.asScala.toList.map(k => js1.get(k).toString).filter(_.startsWith("$")).map(_.substring(1))

    val js = new JSONObject(incoming)
    import scala.collection.JavaConverters._

    // todo just one level deep for now - a flat JSON
    val vals = js.keys.asScala.toList.map(k => (k, js.get(k).toString)).toMap

    // from template spec
    val xinSpecs = inSpecs.map(p => (p.name, p.currentStringValue, p.expr.mkString))

    // from parsed template json
    val foundSpecs = out.map(p => (p, "", ""))

    // distinct by name
    val allSpecs = (xinSpecs ::: foundSpecs).groupBy(_._1).map(_._2.head)

    var parms =
      (for (g <- allSpecs)
          yield (
            g._1,
            Try {
              vals(g._1)
            }.getOrElse(g._2)
          )
        ).filter(_._2 != null).toMap

    parms
    }

  /** parse incoming message with pattern matching */
  def parseIncomingMatch (incoming:String, inSpecs : List[P], incomingMetas: NVP) : NVP = {
    parseIncomingTemplate (this.content, incoming, inSpecs, incomingMetas, false)
  }

  /** parse incoming message with pattern matching */
  def parseIncomingTemplate (content:String, incoming:String, inSpecs : List[P], incomingMetas: NVP, expandAll:Boolean = false) : NVP = {
    // turn template into regex
    var re = content
    // all {} spec chars
//    re = re.replaceAll("""([{}])""", "\\\\$1")
    // all ${xx} expressions
    re = re.replaceAll("""\$\{(.+)\}""", "$1")
    // all $xx expressions
    re = re.replaceAll("""\$(\w+)""", "(?<$1>.*)?")
    // all EOLs
    re = re.replaceAll("""[\r\n ]""", """\\s*""")
    re = "(?sm)" + re + ".*"

    // from parsed template json
    val foundSpecs = if(expandAll) {
      val x = mutable.ListBuffer[String]()

      val PATT = """(\$\w+)*""".r
      val u = PATT.replaceSomeIn(content, { m =>
        val n = if (m.matched.length > 0) m.matched.substring(1) else ""
        if(n != null && n.trim.length > 0) x.append(n.trim)
        None
      })

      x.toList.map(x=> (x,"", ""))
    } else Nil

    val xinSpecs = inSpecs.map(p => (p.name, p.currentStringValue, p.expr.mkString))

    // distinct by name
    val allSpecs = (xinSpecs ::: foundSpecs).groupBy(_._1).map(_._2.head)

    // find and make message with parms
    val jrex = Pattern.compile(re).matcher(incoming)
    val hasit = jrex.find()
    var parms = if (hasit)
      (
          for (g <- allSpecs)
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

  /** parse template and find all parameters */
  def findTemplateParms (content:String) : List[String] = {
//      val PATT = """\$\w+""".r
    val PATT = """\$\{(\w+)\}""".r

    val parms = for(x <- PATT.findAllIn(content).matchData) yield {
      x.group(1)
    }

    parms.toList
  }
}


