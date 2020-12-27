/*  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine.exec

import org.apache.commons.codec.digest.DigestUtils
import razie.clog
import razie.diesel.Diesel
import razie.diesel.dom.RDOM._
import razie.diesel.dom.{RDOM, _}
import razie.diesel.engine.DomEngineSettings.DIESEL_USER_ID
import razie.diesel.engine._
import razie.diesel.engine.nodes._
import razie.diesel.expr.{AExprFunc, ECtx, StaticECtx}
import razie.diesel.model.DieselMsg
import razie.tconf.DUsers
import razie.tconf.hosting.Reactors
import razie.wiki.Base64
import razie.wiki.parser.CsvParser
import scala.collection.concurrent.TrieMap
import scala.collection.mutable.{HashMap, ListBuffer}
import scala.util.Try

object EECtx {
  final val CTX = "ctx"
}

/** executor for "ctx." messages - operations on the current context */
class EECtx extends EExecutor(EECtx.CTX) {

  import razie.diesel.engine.exec.EECtx.CTX

  /** map of active contexts per transaction */
  val contexts = new TrieMap[String, ECtx]()

  override def isMock: Boolean = true

  override def test(ast: DomAst, m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) = {
    m.entity == CTX // todo why not && messages.exists(_.met == m.met)
  }

  override def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] = {
    // todo I don't think i need this - the context should be a static msg context with all those anyways
    def parm(s: String): Option[P] = in.attrs.find(_.name == s).orElse(ctx.getp(s))

    in.met match {

      case "regex" => {
        val payload = in.attrs.find(_.name == Diesel.PAYLOAD).getOrElse(ctx.getRequiredp(Diesel.PAYLOAD))
        val re = ctx.getp("regex").orElse(in.attrs.headOption)
        if(re.isEmpty) {
          List(EError("Need at least a regex parameter"))
        } else {
          // for regex matches, use each capture group and set as parm in context
          // extract parms
          val groups = EContent.extractRegexParms(re.get.calculatedValue, payload.calculatedValue)

          groups.map(t => EVal(P(t._1, t._2)))
        }
      }

      case "persisted" => {
        contexts.get(ctx("kind") + ctx("id")).map(x =>
          if (ctx != x)
            ctx.root.overwrite(x)
        ).getOrElse {
          contexts.put(ctx("kind") + ctx("id"), ctx.root) // should I save this one?
        }
        Nil
      }

      case "log" => {
        clog << "DIESEL.log " + ctx.toString
        Nil
      }

      case "info" => {
        in.attrs.headOption.toList.map(p =>
          EInfo(p.name + " - click me", p.calculatedValue)
        )
      }

      case "test" => {
        clog << "DIESEL.test " + ctx.toString

        Nil
      }

      case "clear" => {
        ctx.clear
        Nil
      }

      case "reset" => {
        ctx.clear
        Nil
      }

      case "map" => {
        // l can be a constant with another parm name OR the actual array
        val list = {
          val l = ctx.getRequiredp("list").calculatedP
          if(l.isOfType(WTypes.wt.ARRAY)) {
            l
          } else if(l.isOfType(WTypes.wt.STRING)) {
            ctx.getRequiredp(l.currentStringValue)
          } else {
            P("", "", WTypes.wt.UNDEFINED) //throw new IllegalArgumentException(s"Can't source input list: $ctxList")
          }
        }

        val ea = parm("msg").get.currentStringValue
        val EMsg.REGEX(e, m) = ea

        val x =
          if(list.calculatedTypedValue.contentType == WTypes.ARRAY)
            list.calculatedTypedValue.asArray
        else if(list.calculatedTypedValue.contentType == WTypes.UNDEFINED)
            Nil
        else
            razie.js.parse(s"{ list : ${list.calculatedValue} }").apply("list")

        x match {
          case l: collection.Seq[Any] => {
            val nat = in.attrs.filter(e => !Array("list", "item", "msg").contains(e.name))
            val res = l.map { item: Any =>
              val itemP = P.fromTypedValue(parm("item").get.currentStringValue, item)
              val args = itemP :: nat
              val out = AExprFunc(ea, args).applyTyped("")
              out.calculatedTypedValue.value
            }
            List(EVal(P.fromTypedValue(Diesel.PAYLOAD, res)))
          }
          case x@_ => {
            List(EError("map works on lists/arrays, found a: " + x.getClass.getName))
          }
        }
      }

      case "foreach" => {
        var info : List[Any] = Nil

        // l can be a constant with another parm name OR the actual array
        val list = {
          val l = ctx.getRequiredp("list").calculatedP
          if(l.isOfType(WTypes.wt.ARRAY)) {
            l
          } else if(l.isOfType(WTypes.wt.STRING)) {
            ctx.getRequiredp(l.currentStringValue)
          } else {
            info = EWarning(s"Can't source input list - what type is it? ${l}") :: info
            P("", "", WTypes.wt.UNDEFINED) //throw new IllegalArgumentException(s"Can't source input list: $ctxList")
          }
        }

        val EMsg.REGEX(e, m) = parm("msg").get.currentStringValue
        val itemName = parm("item").get.currentStringValue

        razie.js.parse(s"{ list : ${list.currentStringValue} }").apply("list") match {
          case l: collection.Seq[Any] => {
            // passing any other parameters that were given to foreach
            val nat = in.attrs.filter(e => !Array("list", "item", "msg").contains(e.name))

            l.map { item: Any =>
              // for each item in list, create message
              val itemP = P.fromTypedValue(itemName, item)
              EMsg(e, m, itemP :: nat)
            }.toList ::: info
          }
          case x@_ => {
            List(EError("value to iterate on was not a list", x.getClass.getName) :: info)
          }
        }
      }

        // nice print of either input parms of default payload
      case "echo" => {
        val toPrint = if (in.attrs.nonEmpty) in.attrs else ctx.getp(Diesel.PAYLOAD).toList

        val res = toPrint.map { p =>
          EInfo(p.toString, p.calculatedTypedValue.asNiceString)
        }

        if (res.isEmpty) List(EInfo("No arguments with values found...")) else res
      }

      case "setVal" => {
        val n = in.attrs.find(_.name == "name").map(_.currentStringValue)
        val v = in.attrs.find(_.name == "value")

        // at this point the
        val res = n.flatMap { name =>
          if (v.exists(_.hasCurrentValue))
            Some(new EVal(name, v.get.currentStringValue))
          else if (v.exists(_.expr.isDefined))
            Some(new EVal(v.get.expr.get.applyTyped("").copy(name = name)))
          else if (v.exists(_.ttype != WTypes.wt.UNDEFINED))
            Some(new EVal(name, v.get.currentStringValue)) // for set (x="")
          else {
            // clear it
            def clear(c: ECtx) {
              c.remove(name)
              c.base.foreach(clear)
            }

            clear(ctx)
            Some(new EInfo("removed " + name))
          }
        }.orElse {
          v.map(_.calculatedP) // just v - copy it
        }.toList

        res.collect {
          case ev: EVal => ctx.put(ev.p)
        }

        res
      }

      case "set" => {
        // set all parms passed in - return EVals and make sure they're set in context
        // important to know how the scope contexts work
        val res = in.attrs.map { p =>
          if (p.hasCurrentValue) // calculated already
            Some(new EVal(p))
          else if (p.expr.isDefined) // calculate now
            Some(new EVal(p.expr.get.applyTyped("").copy(name = p.name)))
          else if (p.ttype != WTypes.wt.UNDEFINED)
            Some(new EVal(p)) // set(x="") is not undefined...
          else {
            // clear it
            ctx.remove(p.name)
            None
          }
        }.filter(_.isDefined).map(_.get)

        res.foreach(v=> ctx.put(v.p))
        res
      }

      case "setAll" => {
        // input is json - set all fields as ctx vals
        val res = in.attrs.map(_.calculatedP).filter(_.ttype == WTypes.JSON).flatMap {p=>
            p.calculatedTypedValue.asJson.map {t=>
              new EVal(P.fromTypedValue(t._1, t._2))
            }
        }

        res.foreach(v=> ctx.put(v.p))
        res
      }

      case "debug" => {
        cdebug(in.attrs)
      }

      // debug current context
      case "trace" => {
        EInfo("All flattened:") ::
            cdebug(in.attrs) ::
            EInfo("-----------detailed tree:") ::
            ctrace(ctx)
      }

        // url safe version
      case "urlbase64encode" => {
        val res = in.attrs.filter(_.name != Diesel.RESULT).map { a =>
          import org.apache.commons.codec.binary.Base64
          def enc(s: String) = new Base64(true).encode(s.getBytes)

          val res = enc(a.calculatedValue)
          new EVal(a.name, new String(res).replaceAll("\n", "").replaceAll("\r", ""))
        }

        res ::: res.headOption.map(x=> x.copy(p=x.p.copy(name=Diesel.PAYLOAD))).toList
      }

        // normal base64 encoder
      case "base64encode" => {
        val res = in.attrs.filter(_.name != Diesel.RESULT).map { a =>
          import org.apache.commons.codec.binary.Base64
          def enc(s: String) = new Base64(false).encode(s.getBytes)

          val res = enc(a.calculatedValue)
          new EVal(a.name, new String(res).replaceAll("\n", "").replaceAll("\r", ""))
        }

        res ::: res.headOption.map(x=> x.copy(p=x.p.copy(name=Diesel.PAYLOAD))).toList
      }

      // take all args and create a json doc with them
      case "mkString" => {
        val pre  = ctx.getp("pre").map(_.calculatedValue).getOrElse("")
        val sep  = ctx.getp("separator").map(_.calculatedValue).getOrElse(",")
        val post = ctx.getp("post").map(_.calculatedValue).getOrElse("")

        // l can be a constant with another parm name OR the actual array
        val list = {
          val l = ctx.getp("list").getOrElse(ctx.getRequiredp(Diesel.PAYLOAD)).calculatedP
          if(l.isOfType(WTypes.wt.ARRAY)) {
            val arr = l.calculatedTypedValue.asArray
            arr
          } else {
//            info = EWarning(s"Can't source input list - what type is it? ${l}") :: info
            throw new IllegalArgumentException(s"Can't source input list: $l")
          }
        }

        val rows = list.map { obj =>
          PValue(obj).asString
        }.mkString(pre, sep, post)

        new EVal(
          RDOM.P.fromTypedValue(Diesel.PAYLOAD, rows, WTypes.wt.STRING)
        ) :: Nil
      }

      // take all args and create a json doc with them
      case "csv" | "jsonToCsv" => {
        val separator = ctx.getRequired("separator")
        val useHeaders = ctx.get("useHeaders").getOrElse("true").toBoolean

        // l can be a constant with another parm name OR the actual array
        val list = {
          val l = ctx.getp("list").getOrElse(ctx.getRequiredp(Diesel.PAYLOAD)).calculatedP
          if (l.isOfType(WTypes.wt.ARRAY)) {
            val arr = l.calculatedTypedValue.asArray
            arr
          } else {
//            info = EWarning(s"Can't source input list - what type is it? ${l}") :: info
            throw new IllegalArgumentException(s"Can't source input list: $l")
          }
        }

        // collecting field names here to avoid empty
        var inames = new collection.mutable.ListBuffer[String]()

        val objects = list.map { obj =>
          val m = PValue(obj).asJson
//          val m = p.calculatedTypedValue.asJson
          // collect new names
          inames.appendAll(m.keys.filter(x => !inames.contains(x)))
          m
        }.toList

        val names = inames.toList

        // collect new names
        var rows = objects.map { m =>
          names.map { n =>
            m
                .get(n)
                .filter(P.isSimpleType)
                .map(x => {

                  if (P.isSimpleNonStringType(x)) {
                    P.asString(x)
                  } else {
                    "\"" + {
                      val s = P.asString(x)
                      s
                          .replaceAll("\"", "\"\"")
//                      .replaceAll(separator, "\"" + separator + "\"")
                    } + "\""
                  }

                }
                )
                .getOrElse("")
          }.mkString(separator)
        }

        rows = (if (useHeaders) List(names.mkString(separator)) else Nil) ++ rows

        new EVal(
          RDOM.P.fromTypedValue(Diesel.PAYLOAD, rows, WTypes.wt.ARRAY)
        ) ::
            new EVal(
              RDOM.P.fromTypedValue("csvHeaders", names, WTypes.wt.ARRAY)
            ) :: Nil
      }

      // incoming csv parsed into json, based on header field.
      // if no header, fields will be "col0"..."colN"
      case "csvToJson" => {
        val separator = ctx.getRequired("separator")
        val hasHeaders = ctx.get("hasHeaders").getOrElse("true").toBoolean
        val payload = ctx.getRequired(Diesel.PAYLOAD)
        var headers = new Array[String](0)

        val result: ListBuffer[Any] = new ListBuffer[Any]()

        // 1. parse into lines
        val parser = new CsvParser() {
          def doit(s: String, delim: String) = {
            parseAll(csv(separator), payload) match {
              case Success(value, _) => value.filter(_.nonEmpty)
              case NoSuccess(msg, _) => {
                result.append(EError(msg, msg))
                Nil
              }
              //todo ? throw new DieselExprException("Parsing error: " + msg)
            }
          }

          def consts(s: String) = {
            parseAll(csvnumConst, s) match {
              case Success(value, _) => value
              case NoSuccess(msg, _) => {
                s // not matched, keep it
              }
              //todo ? throw new DieselExprException("Parsing error: " + msg)
            }
          }
        }

        var lines: List[List[String]] = parser.doit(payload, separator)

        if (hasHeaders) {
          headers = lines.head.toArray
          lines = lines.drop(1)
        }

        val res = lines.map(l => {
          val m = new HashMap[String, Any]()
          l.zipWithIndex.foreach(x => {
            val k = if (hasHeaders) headers(x._2) else "col" + x._2

            var v = x._1

            if (v.trim.startsWith("\"")) {
              val y = v.replaceFirst("^\"", "").replaceFirst("\"$", "")
              m.put(k, y)
            } else {
              val y = parser.consts(v)
              if (y != null) {
                m.put(k, y)
              }
            }

          })

          m
        })

        result.append(
          new EVal(RDOM.P.fromTypedValue(Diesel.PAYLOAD, res.toList, WTypes.wt.ARRAY))
        )

        result.toList
      }

      // take all args and create a json doc with them
      case "json" => {
        val res = in.attrs.map(a => (a.name, a.calculatedTypedValue.value)).toMap

        new EVal(
          RDOM.P.fromTypedValue(Diesel.PAYLOAD, res, WTypes.wt.JSON)
        ) :: Nil
      }

      case "base64decode" => {
        val res = in.attrs.filter(_.name != Diesel.RESULT).map { a =>
          val res = Base64.dec(a.calculatedValue)
          new EVal(RDOM.P(a.name, "", WTypes.wt.BYTES, None, "",
            Some(PValue[Array[Byte]](res, "application/octet-stream"))))
        }

        res ::: res.headOption.map(x=> x.copy(p=x.p.copy(name=Diesel.PAYLOAD))).toList
      }

      case "sha1" => {
        val res = in.attrs.filter(_.name != Diesel.RESULT).map { a =>
          val md = java.security.MessageDigest.getInstance("SHA-1")
          val s = md.digest(a.currentStringValue.getBytes("UTF-8")).map("%02X".format(_)).mkString
//          val sb = DigestUtils.sha1Hex(a.dflt)
          new EVal(a.name + "_sha1", s) //:: new EVal(a.name+"_sha1j", sb) :: Nil
        }

        res :::
            in.attrs
                .find(_.name == Diesel.RESULT)
                .map(_.calculatedValue)
                .orElse(Some(Diesel.PAYLOAD))
                .map(p => new EVal(p, res.head.p.currentStringValue))
                .toList
      }

      case "sha256" => {
        val res = in.attrs.filter(_.name != Diesel.RESULT).map { a =>
          val md = java.security.MessageDigest.getInstance("SHA-256")
          val s = DigestUtils.sha256Hex(a.currentStringValue)
          new EVal(a.name + "_sha256", s)
        }

        res :::
            in.attrs
                .find(_.name == Diesel.RESULT)
                .map(_.calculatedValue)
                .orElse(Some(Diesel.PAYLOAD))
                .map(p => new EVal(p, res.head.p.currentStringValue))
                .toList
      }

      case "timer" => {
        val d = in.attrs.find(_.name == "duration").map(_.currentStringValue.toInt).getOrElse(1000)
        val m = in.attrs.find(_.name == "msg").map(_.currentStringValue).getOrElse(
          "$msg ctx.echo (msg=\"timer without message\")")

        DieselAppContext ! DEStartTimer("x", d, Nil)

        new EInfo("ctx.timer - start " + d) :: Nil
      }

      case "sleep" => {

        /*
        this is not just asynchronous - but also
        1. suspends the engine
        2. ask the engine to send itself a continuation later DELater
        3. continuation DEComplete
         */

        val d = in.attrs.find(_.name == "duration").map(_.calculatedTypedValue.asInt).getOrElse(1000)
        EInfo("ctx.sleep - slept " + d) ::
            EEngSuspend("ctx.sleep", "", Some((e, a, l) => {
              DieselAppContext ! DELater(e.id, d, DEComplete(e.id, a.id, recurse = true, l, Nil))
            })) ::
            Nil
      }

      case "authUser" => {
        val uid =
        // the engine got it from the session/cookie
          ctx.root.engine.flatMap(_.settings.userId) orElse
              // or some test set it
              ctx.get(DIESEL_USER_ID)

        // todo lookup the user - either in DB or wix
        if (uid.isDefined)
          new EInfo("User is auth ") :: Nil
        else
          new EVal(DieselMsg.HTTP.STATUS, "401") ::
          new EVal(Diesel.PAYLOAD, "Error: User not auth") :: // payload will be shown, needs reset
              new EError(s"ctx.authUser - User not auth") ::
              new EEngStop(s"User not auth") :: Nil
      }

      case "setAuthUser" => setAuthUser(ctx)

      case s@_ => {
        new EError(s"ctx.$s - unknown activity ") :: Nil
      }
    }
  }

  def setAuthUser (ctx:ECtx) = {
    /** run tests in the context of a user, configurable per domain */

    // was this engine triggered for a user ? like in a fiddle? Use that one
    val root = ctx.root
    val uid = ctx.root.engine.flatMap(_.settings.userId)

    if (uid.isEmpty) {
      // if no auth user, use the default - same default used for xapikey auth
      val uid =
        root.settings.realm
          .map(Reactors.impl.getProperties)
          .flatMap(_.get("diesel.xapikeyUserEmail"))
          .flatMap(DUsers.impl.findUserByEmailDec)
          .map(_.id)
          .getOrElse(
            "4fdb5d410cf247dd26c2a784" // an inactive account: Harry
          )

      ctx.put(P(DIESEL_USER_ID, uid))
      new EInfo("User is now auth ") :: Nil
    } else
      new EInfo("User was already auth ") :: Nil
  }

  // debug current context
  def cdebug(in: List[P])(implicit ctx:ECtx) : List[EInfo] = {
    in.map { p =>
      new EInfo(s"${p.name} = ${p.currentStringValue} expr=(${p.expr}) cv= ${p.calculatedValue}")
    } :::
        (new EInfo(s"Ctx: ${ctx.getClass.getName}") ::
        ctx.listAttrs.map { p =>
            Try {
              new EInfo(s"${p.name} = ${p.currentStringValue} expr=(${p.expr}) cv= ${p.calculatedValue}")
            }.recover{
              case ex => new EInfo(s"${p.name} = ${p.currentStringValue} expr=(${p.expr}) cv= EXCEPTION: $ex")
            }.get
        })
  }

  // trace current context
  def ctrace(c:ECtx)(implicit ctx:ECtx) : List[Any] = {
    EInfo("--------") ::
        (c match {
      case sc: StaticECtx => {
        EInfo(sc.toString) :: Nil
      }
      case r: DomEngECtx => {
        EInfo(r.toString) :: Nil
      }
      case c@_ => {
        EInfo(c.toString) :: Nil
      }
    }) :: c.base.toList.flatMap(ctrace)
  }


  override def toString = "$executor::ctx "

  override val messages: List[EMsg] =
    EMsg(CTX, "persisted") ::
        EMsg(CTX, "log") ::
        EMsg(CTX, "echo") ::
        EMsg(CTX, "test") ::
        EMsg(CTX, "storySync") :: // processed by the story teller
        EMsg(CTX, "storyAsync") :: // processed by the story teller
        EMsg(CTX, "clear") ::
        EMsg(CTX, "reset") ::
        EMsg(CTX, "timer") ::
        EMsg(CTX, "sleep") ::
        EMsg(CTX, "set") ::
        EMsg(CTX, "setVal") ::
        EMsg(CTX, "setAll") ::
        EMsg(CTX, "sha1") ::
        EMsg(CTX, "sha256") ::
        EMsg(CTX, "foreach") ::
        EMsg(CTX, "trace") ::
        EMsg(CTX, "debug") ::
        EMsg(CTX, "authUser") ::
        EMsg(CTX, "setAuthUser") ::
        EMsg(CTX, "json") ::
        EMsg(CTX, "csv") ::
        EMsg(CTX, "mkString") ::
        Nil
}
