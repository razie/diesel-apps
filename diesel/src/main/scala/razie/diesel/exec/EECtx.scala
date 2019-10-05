/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package mod.diesel.model.exec

import razie.clog
import razie.diesel.dom.RDOM._
import razie.diesel.dom.{RDOM, _}
import razie.diesel.engine.DomEngineSettings.DIESEL_USER_ID
import razie.diesel.engine._
import razie.diesel.exec.EExecutor
import razie.diesel.expr.AExprFunc
import razie.diesel.ext.{MatchCollector, _}
import razie.diesel.{Diesel, ext}
import razie.tconf.DUsers
import razie.tconf.hosting.Reactors
import razie.wiki.Base64
import scala.collection.mutable
import scala.util.Try

object EECtx {
  final val CTX = "ctx"
}

/** executor for "ctx." messages - operations on the current context */
class EECtx extends EExecutor(EECtx.CTX) {

  import EECtx.CTX

  /** map of active contexts per transaction */
  val contexts = new mutable.HashMap[String, ECtx]()

  override def isMock: Boolean = true

  override def test(m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) = {
    m.entity == CTX
  }

  override def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] = {
    def parm(s: String): Option[P] = in.attrs.find(_.name == s).orElse(ctx.getp(s))

    in.met match {

      case "persisted" => {
        contexts.get(ctx("kind") + ctx("id")).map(x =>
          if (ctx != x)
            ctx.root.asInstanceOf[DomEngECtx].overwrite(x)
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

      case "engineSync" => {
        // turn the engine sync
        ctx.root.asInstanceOf[DomEngECtx].engine.map(_.synchronous = true)
        Nil
      }

      case "map" => {
        // l can be a constant with another parm name OR the actual array
        val list = {
          val l = ctx.getRequiredp("list").calculatedP
          if(l.isOfType(WTypes.ARRAY)) {
            l
          } else if(l.isOfType(WTypes.STRING)) {
            ctx.getRequiredp(l.currentStringValue)
          } else {
            P("", "", WTypes.UNDEFINED) //throw new IllegalArgumentException(s"Can't source input list: $ctxList")
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
          case l: List[Any] => {
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
        // l can be a constant with another parm name OR the actual array
        val list = {
          val l = ctx.getRequiredp("list").calculatedP
          if(l.isOfType(WTypes.ARRAY)) {
            l
          } else if(l.isOfType(WTypes.STRING)) {
            ctx.getRequiredp(l.currentStringValue)
          } else {
            P("", "", WTypes.UNDEFINED) //throw new IllegalArgumentException(s"Can't source input list: $ctxList")
          }
        }

        val EMsg.REGEX(e, m) = parm("msg").get.currentStringValue
        val itemName = parm("item").get.currentStringValue

        razie.js.parse(s"{ list : ${list.currentStringValue} }").apply("list") match {
          case l: List[Any] => {
            // passing any other parameters that were given to foreach
            val nat = in.attrs.filter(e => !Array("list", "item", "msg").contains(e.name))

            l.map { item: Any =>
              // for each item in list, create message
              val itemP = P.fromTypedValue(itemName, item)
              EMsg(e, m, itemP :: nat)
            }
          }
          case x@_ => {
            List(EError("list was not a list", x.getClass.getName))
          }
        }
      }

        // nice print of either input parms of default payload
      case "echo" => {
        val toPrint = if (in.attrs.size > 0) in.attrs else ctx.getp(Diesel.PAYLOAD).toList

        toPrint.map { p =>
          EInfo(p.toString, p.calculatedTypedValue.asNiceString)
        }
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
          else if (v.exists(_.ttype != WTypes.UNDEFINED))
            Some(new EVal(name, v.get.currentStringValue)) // for set (x="")
          else {
            // clear it
            def clear(c: ECtx): Unit = {
              c.remove(name)
              c.base.map(clear)
            }

            clear(ctx)
            Some(new EInfo("removed " + name))
          }
        }.orElse {
          v.map(_.calculatedP) // just v - copy it
        }.toList

        res.collect {
          case ev:EVal => ctx.put(ev.p)
        }.toList

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
          else if (p.ttype != WTypes.UNDEFINED)
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

      case "base64encode" => {
        val res = in.attrs.filter(_.name != "result").map { a =>
          val res = Base64.enc(a.calculatedValue)
          new EVal(a.name, new String(res).replaceAll("\n", ""))
        }

        res ::: res.headOption.map(x=> x.copy(p=x.p.copy(name=Diesel.PAYLOAD))).toList
      }

      // take all args and create a json doc with them
      case "json" => {
        val res = in.attrs.map(a => (a.name, a.calculatedTypedValue.value)).toMap

        new EVal(
          RDOM.P.fromTypedValue(Diesel.PAYLOAD, res, WTypes.JSON)
        ) :: Nil
      }

      case "base64decode" => {
        val res = in.attrs.filter(_.name != "result").map { a =>
          val res = Base64.dec(a.calculatedValue)
          new EVal(RDOM.P(a.name, "", WTypes.BYTES, "", "", None,
            Some(PValue[Array[Byte]](res, "application/octet-stream"))))
        }

        res ::: res.headOption.map(x=> x.copy(p=x.p.copy(name=Diesel.PAYLOAD))).toList
      }

      case "sha1" => {
        val res = in.attrs.filter(_.name != "name").map { a =>
          val md = java.security.MessageDigest.getInstance("SHA-1")
          val s = md.digest(a.currentStringValue.getBytes("UTF-8")).map("%02X".format(_)).mkString
//          val sb = DigestUtils.sha1Hex(a.dflt)
          new EVal(a.name + "_sha1", s) //:: new EVal(a.name+"_sha1j", sb) :: Nil
        }

        res :::
            in.attrs
                .find(_.name == "name")
                .map(_.calculatedValue)
                .map(p => new EVal(p, res.head.p.currentStringValue))
                .toList

//        new EVal(a.name+"_sha1", s) :: new EVal("result", s) :: Nil
      }

      case "timer" => {
        val d = in.attrs.find(_.name == "duration").map(_.currentStringValue.toInt).getOrElse(1000)
        val m = in.attrs.find(_.name == "msg").map(_.currentStringValue).getOrElse("$msg ctx.echo (msg=\"timer without message\")")
        DieselAppContext.router.map(_ ! DEStartTimer("x", d, Nil))
        new EInfo("ctx.timer - start " + d) :: Nil
      }

      case "sleep" => {

        /*
        this is not just asynchronous - but also
        1. suspends the engine
        2. ask the engine to send itself a continuation later DELater
        3. continuation DEComplete
         */

        val d = in.attrs.find(_.name == "duration").map(_.currentStringValue.toInt).getOrElse(1000)
        EInfo("ctx.sleep - slept " + d) ::
            ext.EEngSuspend("ctx.sleep", "", Some((e, a, l) => {
              DieselAppContext.router.map(_ ! DELater(e.id, d, DEComplete(e.id, a, true, l, Nil)))
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
          new EVal("diesel.response.http.code", "401") ::
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
        EMsg(CTX, "engineSync") ::
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
        EMsg(CTX, "foreach") ::
        EMsg(CTX, "trace") ::
        EMsg(CTX, "debug") ::
        EMsg(CTX, "authUser") ::
        EMsg(CTX, "setAuthUser") ::
        EMsg(CTX, "json") ::
        Nil
}
