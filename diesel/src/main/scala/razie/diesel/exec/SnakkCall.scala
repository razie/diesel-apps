/**
  * ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package razie.diesel.exec

import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import java.net.Socket
import java.util.regex.Pattern

import com.novus.salat._
import com.razie.pub.comms.Comms
import razie.db.RazSalatContext._
import razie.diesel.dom.RDOM._
import razie.diesel.engine.InfoAccumulator
import razie.diesel.ext._
import razie.tconf.DTemplate
import razie.{Snakk, SnakkRequest, SnakkResponse, SnakkUrl}

import scala.Option.option2Iterable
import scala.concurrent.{Awaitable, Future, Promise}
import scala.util.Try
import scala.xml.Node

/** a single snakk call to make
  *
  * @param protocol http, telnet
  */
case class SnakkCall(protocol: String, method: String, url: String, headers: Map[String, String], content: String, template:Option[DTemplate] = None) {
  def toJson = grater[SnakkCall].toPrettyJSON(this)

  def toSnakkRequest (id:String="") = SnakkRequest(protocol, method, url, headers, content, id)

  var pro : Option[Promise[SnakkResponse]] = None

  var isurl: SnakkUrl = null

  def setUrl(u: SnakkUrl) = {
    isurl = u
  }

  def postContent = if (content != null && content.length > 0) Some(content) else None

  var ibody: Option[String] = None
  var iContentType: Option[String] = None

  def body = ibody getOrElse makeCall

  def makeCall = {
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

  def future : Future[SnakkResponse] = {
    if(! this.pro.isDefined) {
      this.pro = Some(Promise[SnakkResponse]())
      AsyncSnakkCallList.put(this)
    }
    pro.get.future
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


