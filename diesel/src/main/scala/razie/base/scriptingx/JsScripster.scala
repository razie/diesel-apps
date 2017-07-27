/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.base.scriptingx

import javax.script.ScriptEngineManager

import jdk.nashorn.api.scripting.{ClassFilter, NashornScriptEngineFactory}
import razie.audit.Audit
import razie.{CSTimer, Logging, csys}

import scala.util.Try

// todo use MiniScripster and WikiScripster when decoupled
object JsScripster extends Logging {

  //todo deprecate
  def isfiddleMap(script: String, lang: String, q: Map[String, String], typed: Option[Map[String, Any]] = None) =
    newsfiddleMap(script, lang, q, typed, false)

  /** run a fiddle with a map of arguments "queryParms" */
  // todo protect calls to this
  def newsfiddleMap(script: String, lang: String, q: Map[String, String], typed: Option[Map[String, Any]] = None, doAudit: Boolean = true) = {
//    val wix = api.wix(we, au, q, "")
    val c = new CSTimer("script", "?")
    c.start()

    if (lang == "js") {

      val qj = qtojson(q)
//      val jscript = s"""var queryParms = $qj;\n${wix.json}\n$script"""
      val jscript = s"""var queryParms = $qj;\n$script"""
      try {
        //        val factory = new ScriptEngineManager()
        //        val engine = factory.getEngineByName("JavaScript")
        val factory = new NashornScriptEngineFactory();
        val engine = factory.getScriptEngine(new MyCF());

        val bindings = engine.createBindings()
        // attempt to use typed bindings, if available
        q.foreach(t => bindings.put(t._1, typed.flatMap(_.get(t._1)).getOrElse(jstypeSafe(t._2))))
//        bindings.put("wixj", wix)
        val res = engine.eval(jscript, bindings)
        (true, if (res != null) res.toString else "")
      } catch {
        case t: Throwable => {
          log(s"while executing script\n$jscript", t)
          // don't include the script body - security issue
            (false, t /* + "\n\n" + jscript */)
        }
      } finally {
        c.stop()
        audit("SFIDDLE_EXEC JS" + (c.last - c.beg) + " msec" + jscript)
        if (doAudit)
          Audit.logdb("SFIDDLE_EXEC", "JS", (c.last - c.beg) + " msec", jscript.takeRight(300))
      }

    } else if (lang == "ruby") {

      val qj = qtojson(q)
      val jscript = "" //s"""var queryParms = $qj;\n$script"""
      try {
        val factory = new ScriptEngineManager()
        val engine = factory.getEngineByName("rb")
        val res = engine.eval(jscript)
        var bindings = engine.createBindings()
        q.foreach(t => bindings.put(t._1, typed.flatMap(_.get(t._1)).getOrElse(t._2)))
        Audit.logdb("SFIDDLE_EXEC", "ruby", jscript)
        (true, res.toString)
      } catch {
        case t: Throwable => {
          log(s"while executing script\n$jscript", t)
          (false, t + "\n\n" + jscript)
        }
      }

    } else if (lang == "scala") {

      throw new IllegalArgumentException ("scala not supported at this point")

    } else (false, script)
  }

  class MyCF extends ClassFilter {
    override def exposeToScripts(s: String): Boolean = {
      if (s.startsWith("api." /* WixUtils" */)) true;
      else {
        Audit.logdb("ERR_DENIED", "js class access denied ", s)
        false
      }
    }
  }

  def typeSafe(v: String): String = {
    if (v.trim.startsWith("\"") || v.trim.startsWith("'") || v.trim.startsWith("{") || v.trim.startsWith("[")) v
    else Try {
      v.toInt.toString
    } getOrElse {
      "'" + v + "'"
    }
  }

  def jstypeSafe(v: String): Any = {
    if (v.trim.startsWith("\"") || v.trim.startsWith("'")) v.replaceFirst("[\"']([^\"']*)[\"']", "$1")
    else if (v.trim.startsWith("{") || v.trim.startsWith("[")) v
    else Try {
      v.toInt
    } getOrElse {
      v
    } //"'"+v+"'" }
  }

  def qtojson(q: Map[String, String]) = "{" + q.map(t => s"""${t._1} : ${typeSafe(t._2)} """).mkString(",") + "}"

  def qtourl(q: Map[String, String]) = q.map(t => s"""${t._1}=${t._2}""").mkString("&")

}

