/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.base.scriptingx

import api.dwix
import javax.script.ScriptEngineManager
import jdk.nashorn.api.scripting.{ClassFilter, NashornScriptEngineFactory, ScriptObjectMirror}
import org.bson.types.ObjectId
import razie.audit.Audit
import razie.diesel.expr.ECtx
import razie.tconf.DUsers
import razie.{CSTimer, Logging, js}
import scala.util.Try

/** preliminary JS scripster for embedded diesel scripts */
// todo use MiniScripster and WikiScripster when decoupled
object DieselScripster extends Logging {

  val DEBUG = false

  /** run a script with a map of arguments "queryParms"
    *
    * @param script the script content in language
    * @param lang the language (js/ruby/scala)
    * @param q the input parms
    * @param typed typed input parms
    * @param doAudit shoudl we audit this?
    * @param exprs expressions to include in the script content (other functions etc)
    *
    * @return (succ/fail, x.toString, x)
    */
  // todo protect calls to this
  def newsfiddleMap(
    script: String,
    lang: String,
    q: Map[String, String],
    typed: Option[Map[String, Any]] = None,
    doAudit: Boolean = true,
    exprs:Map[String,String] = Map.empty,
    ctx:ECtx) : (Boolean, String, Any) = {

    val c = new CSTimer("script", "?")
    c.start("DieselScripster.newsfiddleMap")

    if (lang == "js") {

      var expressions = exprs.map {
        t=> s"${t._1} = ${t._2} ;\n"
      }.mkString

      var jscript = s"""$expressions\n$script"""
      var offset = expressions.lines.size + 1

      try {
        //        val factory = new ScriptEngineManager()
        //        val engine = factory.getEngineByName("JavaScript")

        val factory = new NashornScriptEngineFactory();
        val engine = factory.getScriptEngine(new MyCF());

        val bindings = engine.createBindings()

        // attempt to use typed bindings, if available
        q.foreach{t =>
          val v = typed.flatMap(_.get(t._1)).getOrElse(jstypeSafe(t._2))
          if(DEBUG) debug("SFIDDLE_EXEC JS bind: " + t._1 + " = " + v.toString.take(100))

          if(v.isInstanceOf[Map[_,_]]) {
            val m = v.asInstanceOf[Map[_, _]]

            // if we have complex values, this won't work... so default back to string
            if(m.values.exists(_.isInstanceOf[collection.Seq[_]]) ||
               m.values.exists(_.isInstanceOf[collection.Map[_,_]])) {
              val ms = js.tojsons(m)
              expressions = expressions + s"${t._1} = $ms ;\n"
            } else
              bindings.put(t._1, js.toJava(v.asInstanceOf[Map[_, _]]))
          }
          else if(v.isInstanceOf[collection.Seq[_]]) {
            val m = v.asInstanceOf[collection.Seq[_]].toList

            // if we have complex values, this won't work... so default back to string
            if (m.exists(_.isInstanceOf[collection.Seq[_]]) ||
                m.exists(_.isInstanceOf[collection.Map[_, _]])) {
              val ms = js.tojsons(m, 0)
              expressions = expressions + s"${t._1} = $ms ;\n"
            } else
              bindings.put(t._1, js.toJava(m))
          }
          else
            bindings.put(t._1, v)
        }

        jscript = s"""$expressions\n$script"""
        offset = expressions.lines.size + 1

        {
          val settings = ctx.root.engine.map(_.settings)
          val au = settings.flatMap(_.userId).map(new ObjectId(_))
              .flatMap(DUsers.impl.findUserById)

          val wix = razie.js toJava Map(
            "diesel" -> Map(
              "env" -> dwix.dieselEnvFor(settings.flatMap(_.realm).mkString, au),
              "user" -> settings.flatMap(_.userId).mkString
            )
          )

          // used typed values. This will resolve a "3" into a numberic 3
//          val qm = q.map(t => (t._1, typed.flatMap(_.get(t._1)).getOrElse(jstypeSafe(t._2))))
//          val qjm = razie.js toJava qm
//          bindings.put("queryParms", qjm)

          bindings.put("wixj", wix)
          bindings.put("wix", wix)
        }

        val res = engine.eval(jscript, bindings)

        if(res != null && res.isInstanceOf[ScriptObjectMirror]) {
          // return objects with nice tostring
          val json = engine.eval("JSON").asInstanceOf[ScriptObjectMirror]
          val s = json.callMember("stringify", res)

          // debug weirdness on 107
//          val jsres = res.asInstanceOf[ScriptObjectMirror]
//          trace("JS result is a " + jsres.getClassName + " AS STRING: " + jsres.toString + " S AS STRING " + s
//          .getClass.getSimpleName + ":" + s.toString)
//          trace("JSON JS result is a " + json.getClassName + " AS STRING: " + json.toString + " S AS STRING " +
//          json.getClass.getSimpleName + ":" + json.toString)
//          val st = json.getMember("stringify")
//          trace("ST JS result is a " + st.getClass.getSimpleName + " AS STRING: " + st.toString + " S AS STRING " +
//          st.getClass.getSimpleName + ":" + st.toString)

          (true, s.toString, res)
        }
        else
          (true, if (res != null) res.toString else "", res)
      } catch {
        case t: javax.script.ScriptException => {
          log(s"Exception while executing script: ${t.getMessage}\n${jscript.takeRight(300)}")
          // don't include the script body - security issue
          (false, s"(line offset:$offset) "+t.getMessage, t)
        }
        case t: Throwable => {
          log(s"Throwable Exception while executing script\n${jscript.takeRight(300)}", t)
          // don't include the script body - security issue
          (false, s"(line offset:$offset) "+t.getMessage, t)
        }
      } finally {

        c.stop()

        audit("xSFIDDLE_EXEC JS (in " + (c.last - c.beg) + " msec) : " + jscript.takeRight(300))
        if (doAudit)
          Audit.logdb("SFIDDLE_EXEC", "JS", (c.last - c.beg) + " msec", jscript.takeRight(300))
      }

    } else if (lang == "ruby") {

      val jscript = "" //s"""var queryParms = $qj;\n$script"""
      try {
        val factory = new ScriptEngineManager()
        val engine = factory.getEngineByName("rb")
        val res = engine.eval(jscript)
        var bindings = engine.createBindings()
        q.foreach(t => bindings.put(t._1, typed.flatMap(_.get(t._1)).getOrElse(t._2)))
        Audit.logdb("SFIDDLE_EXEC", "ruby", jscript)
        (true, res.toString, res)
      } catch {
        case t: Throwable => {
          log(s"while executing script\n$jscript", t)
          // don't include the script body - security issue
          (false, t.toString, t)
        }
      }

    } else if (lang == "scala") {
      ???
//      try {
//        val res = WikiScripster.implScala.runScriptAny(script, lang, None, None, q, true)
//        Audit.logdb("SFIDDLE_EXEC", "scala", script)
//        (true, res.toString, res)
//      } catch {
//        case t: Throwable => {
//          log(s"while executing script\n$script", t)
//                    throw t
//          (false, t.toString, t)
//        }
//    }

    } else (false, script, script)
  }

  class MyCF extends ClassFilter {
    override def exposeToScripts(s: String): Boolean = {
      if (s.startsWith("api." /* WixUtils" */)) true
        // todo wrap this to protect access to cout/csys etc
      else if (s.startsWith("java.lang.System")) true
      else {
        Audit.logdb("ERR_DENIED", "js class access denied ", s)
        false
      }
    }
  }

  /** allow for values that contain " - strip that */
  def typeSafe(v: String): String = {
    if (v.trim.startsWith("\"") || v.trim.startsWith("'") || v.trim.startsWith("{") || v.trim.startsWith("[")) v
    else Try {
      v.toInt.toString
    } getOrElse {
      "'" + v + "'"
    }
  }

  /** allow for values that contain " - strip that */
  def jstypeSafe(v: String): Any = {
    if (v.trim.startsWith("\"") || v.trim.startsWith("'")) v.replaceFirst("[\"']([^\"']*)[\"']", "$1")
    else if (v.trim.startsWith("{") || v.trim.startsWith("[")) v
    else Try {
      v.toInt
    } getOrElse {
      v
    } //"'"+v+"'" }
  }

  def qtourl(q: Map[String, String]) = q.map(t => s"""${t._1}=${t._2}""").mkString("&")

}

