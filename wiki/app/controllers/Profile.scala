package controllers

import admin.Config
import com.google.inject._
import mod.snow.{RacerKidz, _}
import razie.db.{ROne, Txn}
import razie.{Logging, Snakk, cout}
import razie.db.RMongo._
import razie.wiki.Sec._
import model._
import org.bson.types.ObjectId
import org.joda.time.DateTime
import org.json.JSONObject
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.{mapping, nonEmptyText, tuple, _}
import play.api.mvc.{Action, Request}
import razie.OR._
import razie.audit.Audit
import razie.diesel.model.DieselMsgString
import razie.diesel.model.DieselTarget
import razie.hosting.Website
import razie.wiki.admin.{SecLink, SendEmail}
import razie.wiki.model._
import razie.wiki.{Enc, Services}
import play.api.data.Forms._
import play.api.data._


/** this is NOT the controller */
object Profile extends RazController {

  val trusts = Array("Public", "Club", "Friends", "Private")
  val notifiers = Array("Everything", "FriendsOnly", "None")

  final val cNoConsent = new Corr("Need consent!", """You need to <a href="/doe/consent">give your consent</a>!""");

  // TODO this shoud be private
  def updateUser(old: User, newU: User)(implicit request: Request[_]) = {
    old.update(newU)
    cleanAuth(Some(newU))
    newU
  }

  // TODO should be private
  def createUser(u: User)(implicit request: Request[_], txn:Txn) = {
    val created = {u.create(u.mkProfile); Some(u)}

    // todo decouple with the event below
    created.foreach { x => RacerKidz.myself(x._id) }

    created.foreach {x=>
      Services ! UserCreatedEvent(x._id.toString, Config.node)
    }

    UserTasks.verifyEmail(u).create

    SendEmail.withSession(Website.realm(request)) { implicit mailSession =>
      Tasks.sendEmailVerif(u)
      val uname = (u.firstName + (if (u.lastName.length > 0) ("." + u.lastName) else "")).replaceAll("[^a-zA-Z0-9\\.]", ".").replaceAll("[\\.\\.]", ".")
      Emailer.sendEmailUname(uname, u, false)
      Emailer.tellAdmin("New user", u.userName, u.emailDec)
    }
    created
  }
}

case class UserCreatedEvent (uid:String, node:String) extends WikiEventBase

/** temporary registration/login form */
case class Registration(email: String, password: String, reemail:String="", repassword: String = "") {
  def ename = email.replaceAll("@.*", "")
}

// create profile
case class CrProfile(firstName: String, lastName: String, yob: Int, address: String, userType: String, accept: Boolean, g_recaptcha_response: String="", about:String="")

@Singleton
class Profile @Inject() (config:Configuration) extends RazController with Logging {

  def registerForm (implicit request : Request[_]) = Form {
    mapping(
      "email" -> nonEmptyText.verifying("Wrong format!", vldEmail(_)).verifying("Invalid characters", vldSpec(_)),
      "password" -> nonEmptyText.verifying("Too short!", p => (p.length == 0 || p.length >= 4)),
      "reemail" -> text,
      "repassword" -> text
      )(Registration.apply)(Registration.unapply) verifying
      ("Email mismatch - please type again", { reg: Registration =>
        if (reg.reemail.length > 0 && reg.email.length > 0 && reg.email != reg.reemail) false
        else true
      }) verifying
      ("Password mismatch - please type again", { reg: Registration =>
        if (reg.password.length > 0 && reg.repassword.length > 0 && reg.password != reg.repassword) false
        else true
      }) verifying
      ("Wrong email or password - please type again. (To register a new account, enter the password twice...)", { reg: Registration =>
        //          println ("======="+reg.email.enc+"======="+reg.password.enc)
        if (reg.password.length > 0 && reg.repassword.length <= 0)
          // TODO optimize - we lookup users twice on loing
          Users.findUserByEmailDec(reg.email).orElse(Users.findUserNoCase(reg.email)).map { u =>
            //          println ("======="+u.email+"======="+u.pwd)
            if (reg.password.enc == u.pwd) true
            else {
              u.auditLoginFailed (Website.getRealm)
              false
            }
          } getOrElse {
            Audit.wrongLogin(reg.email, reg.password)
            false
          }
        else true
      }) verifying
      ("Email already registered - if you are logging in, type the password once!", { reg: Registration =>
        if (reg.password.length > 0 && reg.repassword.length > 0)
          Users.findUserByEmailDec(reg.email).map(u => false) getOrElse { true }
        else true
      })
  }

  def crProfileForm(implicit request: Request[_]) = Form {
    mapping(
      "firstName" -> nonEmptyText.verifying("Obscenity filter", !Wikis.hasBadWords(_)).verifying("Invalid characters", vldSpec(_)),
      "lastName" -> text.verifying("Obscenity filter", !Wikis.hasBadWords(_)).verifying("Invalid characters", vldSpec(_)),
      "yob" -> number(min = 1900, max = 2012),
      "address" -> text.verifying("Invalid characters", vldSpec(_)),
      "userType" -> nonEmptyText.verifying("Please select one", ut => Website.userTypes.contains(ut)),
      "accept" -> checked("").verifying("You must accept the Terms of Service to use this site", { x: Boolean => x }),
      "g-recaptcha-response" -> text,
      "about" -> text.verifying("Invalid characters", vldSpec(_))
      )(CrProfile.apply)(CrProfile.unapply) verifying
      ("reCAPTCHA failed!", { cr: CrProfile =>
        new Recaptcha(config).verify2(cr.g_recaptcha_response, clientIp)
      }) verifying
      ("Can't use last name for organizations!", { cr: CrProfile =>
        cr.userType != UserType.Organization.toString || cr.lastName.length <= 0
      })
  }

  // todo stop messing with the routes again
  def doeJoinAlso = doeJoin("", "", "")

  // join step 1
  def doeJoin(club: String, role: String, next: String) = RAction {implicit request=>
    auth // clean theme
    val reg = request.flash.get(SecLink.HEADER).flatMap(SecLink.find).map {sl=>
      Registration(sl.props("email"),"", "", "")
    }.getOrElse (Registration("",""))

    val res = (ROK.r noLayout {implicit stok=> views.html.doeJoin(registerForm.fill(
      reg
    ))}).withSession(
      "gaga" -> System.currentTimeMillis.toString,
      "extra" -> "%s,%s,%s".format(club, role, next))

    // carry on the flash
    request.flash.get(SecLink.HEADER).map(x=> res.addingToSession(SecLink.HEADER -> x)).getOrElse (res)
  } // continue with register()

  // when joining with google first time and have account - enter password
  def doeJoin1Google = Action {implicit request=>
    auth // clean theme
    (ROK.r noLayout {implicit stok=> views.html.doeJoinGoogle(registerForm)}).withSession("gaga" -> request.session.get("gaga").mkString, "extra" -> request.session.get("extra").mkString,  "gid" -> request.session.get("gid").mkString)
  } // continue with register()

  /** join with email */
  def doeJoinWith(email: String) = Action {implicit request=>
    auth // clean theme
    log("joinWith email=" + email)
    (ROK.r noLayout {implicit stok=>
      views.html.doeJoin(
        registerForm.fill(
          Registration(email.dec, "", "", ""))
      )}
      ).withSession(
        "gaga" -> System.currentTimeMillis.toString
    )
  } // continue with register()

  // join step 2 - submited email/pass form
  def doeJoin2 = RAction { implicit request =>
    auth // clean theme
    registerForm.bindFromRequest.fold(
    formWithErrors => (ROK.r badRequest {implicit stok
      => views.html.doeJoin(formWithErrors)}).withSession(
        "gaga" -> System.currentTimeMillis.toString,
        "extra" -> request.session.get("extra").mkString
    ),
    {
      case reg @ Registration(e, p, _, _) => {
        val g = try {
          (request.session.get("gaga").map(identity).getOrElse("1")).toLong
        } catch {
          case _: Throwable => 1
        }

        // allow this only for some minutes
        if (System.currentTimeMillis - g <= 120000) {
          login(reg.email, reg.password, request.session.get("extra").mkString)
        } else {
          Msg2(
            """Session expired - please <a href="/doe/join">start again</a>.
              |<p>Please make sure your browser allows cookies for this website, thank you!""".stripMargin, Some("/doe/join")).withNewSession
        }
      }
    })
  }

  // join step 2 with google - link to existing account
  def doeJoin2Google = RAction { implicit request =>
    auth // clean theme
    registerForm.bindFromRequest.fold(
      formWithErrors => {
        cout << formWithErrors
        (ROK.r badRequest {implicit stok=> views.html.doeJoinGoogle(formWithErrors)}).withSession("gaga" -> request.session.get("gaga").mkString,
          "extra" -> request.session.get("extra").mkString, "gid" -> request.session.get("gid").mkString)
      },
      {
        case reg @ Registration(e, p, _, _) => {
          val g = try {
            (request.session.get("gaga").map(identity).getOrElse("1")).toLong
          } catch {
            case _: Throwable => 1
          }

          // allow this only for some minutes
          if (System.currentTimeMillis - g <= 120000 && request.session.get("gid").isDefined) {
            // associate user to google id and login
            Users.findUserByEmailDec((reg.email)) orElse (Users.findUserNoCase(reg.email)) foreach {u =>
              u.update(u.copy(gid = request.session.get("gid")))
            }
            login(reg.email, reg.password, request.session.get("extra").mkString, request.session.get("gid").mkString)
          } else {
            Msg2(
              """Session expired - please <a href="/doe/join">start again</a>.
                |<p>Please make sure your browser allows cookies for this website, thank you!""".stripMargin, Some("/doe/join")).withNewSession
          }
        }
      })
  }

  /** login or start registration */
  def login(email: String, pass:String, extra: String, gid:String="") (implicit request:RazRequest) = {
    // TODO optimize - we lookup users twice on login
    val realm = Wikie.getRealm()
    val website = request.website
    val secLink = request.session.get(SecLink.HEADER).flatMap(SecLink.find)
    debug("login.secLink="+secLink.mkString)

      Users.findUserByEmailDec((email)) orElse (Users.findUserNoCase(email)) match {
      case Some(u) =>
        if (
          (
            Enc(pass) == u.pwd ||
            u.gid.exists(_ == gid)
          ) &&
          (
            website.openMembership ||
            u.realms.contains(realm) ||
            realm=="rk" ||
            realm=="ski" ||
            realm=="wiki" ||
            u.isAdmin )
        ) {
          Audit.logdb("USER_LOGIN", u.userName, u.firstName + " " + u.lastName + " realm: " + realm)
          u.auditLogin(realm)
          debug("SEss.conn=" + (Services.config.CONNECTED -> Enc.toSession(u.email)))

          // process extra parms to determine what next
          var next : Option[String] = None
          var club : String = ""
          request.session.get("extra") map { x =>
            val s = x split ","
            if (s.size > 0 && s(0).length > 0) {
              val prefix = if (s(0).indexOf(":") > 0) "" else "Club:" // for old links with name instead of WID
              next = Some("/wikie/linkuser/" + prefix + s(0) + "?wc=0")
              club=WID.fromPath(s(0)).map(_.wpathnocats).mkString
            }
          }

          (if(u.profile.flatMap(_.consent).isDefined) {
            if(next.isDefined)
              Msg2( s"""Click below to continue joining the $club.""", next)
            else
              Redirect("/")
          } else
            (ROK.r apply {implicit stok=>
              views.html.user.doeConsent(next.getOrElse("/"))
            })
          ).withSession(Services.config.CONNECTED -> Enc.toSession(u.email))
        } else {
          u.auditLoginFailed(realm)

          if(Enc(pass) == u.pwd || u.gid.exists(_ == gid) ) {
            // user is ok but not member
            Msg2C (
              s"""Oops... you are not a member of this site/project. To join, open a support request.<br>
                 |<br>
                 |<small>Translation: even though you do have an account here, you're not a member of this particular site!</small>
               """.stripMargin, Some(routes.Profile.doeJoin())).withNewSession
          } else {
            // user not ok, try again
            Redirect(routes.Profile.doeJoin()).withNewSession
          }
        }
      case None => // capture basic profile and create profile
        if(request.website.openMembership ||
          (secLink.exists(_.props("realm") == request.realm) &&
            secLink.exists(_.props("email") == email)
          ))
          Redirect(routes.Profile.doeJoin3).flashing("email" -> email, "pwd" -> pass, "gid"->gid, "extra" -> extra)
        else
          Msg2 (s"""This website ($realm) does not allow new accounts""")
    }
  }

  /** logout */
  def doeLogout () = Action { implicit request =>
    Redirect("/").withNewSession
  }

  // display consent
  def doeConsent (next:String) = FAUR { implicit request =>
    ROK.k apply {implicit stok=>
      views.html.user.doeConsent(next)
    }
  }

  // accepted consent, record it
  def doeConsent2 (ver:String, next:String) = FAUR { implicit request =>
      request.au.get.profile.map(p => p.update(p.consented(ver)))
      UserEvent(request.au.get._id, "CONSENTED").create
      cleanAuth()
      Msg2("Thank you!", Some(next))
  }

  /** start registration long form - submit is doeCreateProfile */
  def doeJoin3 = Action { implicit request =>
    auth // clean theme
    (for (
      e <- request.flash.get("email");
      p <- request.flash.get("pwd") orElse request.flash.get("gid")
    ) yield
      (ROK.r reactorLayout12  {implicit stok=>
        views.html.user.doeJoin3(crProfileForm.fill(
          CrProfile("", "", 13, "", "racer", false)))})
        .withSession(
          "pwd" -> p,
          "email" -> e,
          "extra" -> request.flash.get("extra").mkString,
          "gid" -> request.flash.get("gid").mkString
        )
    ) getOrElse
      unauthorized(
        """Session expired [join3] - please <a href="/doe/join">start again</a>."""
        ).withNewSession
  }

  private def dfltCss = Services.config.sitecfg("dflt.css") getOrElse "light"

  /** join step 4 - after captcha: create profile and send emails */
  def doeCreateProfile(testcode: String) = Action { implicit request =>
    implicit val errCollector = new VErrors()

    def getFromSession(s: String, d: String) =
      if (testcode != T.TESTCODE) request.session.get(s)
      else Some(d)

    def unameauto (yob:Int) = Services.config.sitecfg("userName.auto").exists(_ startsWith "ye") && DateTime.now.year.get - yob > 12
    def unameF (f:String,l:String) = (f + (if (l.length > 0) ("." + l) else "")).replaceAll("[^a-zA-Z0-9\\.]", ".").replaceAll("[\\.\\.]", ".")

    def uname (f:String,l:String,yob:Int) =
      if (! unameauto(yob))
        System.currentTimeMillis.toString
      else unameF(f.trim,l.trim)

    auth // clean theme

    val resp = crProfileForm.bindFromRequest

    resp.fold(
      formWithErrors => {
        warn("FORM ERR " + formWithErrors)
        (ROK.r badRequest {implicit stok=> views.html.user.doeJoin3(formWithErrors)}).withSession(
          "pwd" -> getFromSession("pwd", T.TESTCODE).mkString,
          "email" -> getFromSession("email", "@k.com").mkString,
          "extra" -> getFromSession("extra", "extra").mkString,
          "gid" -> request.session.get("gid").mkString)
      },
      {
        //  case class CrProfile (firstName:String, lastName:String, yob:Int, email:String, userType:String)
        case CrProfile(f, l, y, addr, ut, accept, _, about) =>

          (for (
            p <- getFromSession("pwd", T.TESTCODE) orErr ("psession corrupted");
            e <- getFromSession("email", f + l + "@k.com") orErr ("esession corrupted");
            already <- (!Users.findUserByEmailDec((e)).isDefined) orCorr ("User already created" -> "patience, patience...");
            iu <- Some(User(
              uname(f, l, y), f.trim, l.trim, y, Enc(e),
              Enc(p), 'a', Set(ut),
              Set(Website.realm),
              (if (addr != null && addr.length > 0) Some(addr) else None),
              Map("css" -> dfltCss, "favQuote" -> "Do one thing every day that scares you - Eleanor Roosevelt", "weatherCode" -> "caon0696"),
              request.flash.get("gid") orElse request.session.get("gid")))
          ) yield {
            // finally created a new account/profile
            var u = iu
            if (u.isUnder13 && !u.isClub) {
              Redirect(routes.Tasks.addParent1).withSession("ujson" -> u.toJson, "extra" -> request.session.get("extra").mkString, "gid" -> request.flash.get("gid").mkString)
            } else {
              if(about.length > 0) u = u.copy(prefs = u.prefs + ("about" -> about))

              razie.db.tx("doeCreateProfile", u.userName) { implicit txn =>
                // TODO bad code - update and reuse account creation code in Tasks.addParent
                val pro = u.mkProfile
                val created = { u.create(pro); Some(u)}
                created.foreach { x => RacerKidz.myself(x._id) }

                // when testing, skip email verification
                if(! Services.config.isLocalhost) {
                  UserTasks.verifyEmail(u).create
                } else {
                  val ppp = pro.addPerm("+" + Perm.eVerified.s).addPerm("+" + Perm.uWiki.s)
                  pro.update(if (u.isUnder13) ppp else ppp.addPerm("+" + Perm.uProfile.s))
                }

                if (u.isClub) {
                  UserTasks.setupCalendars(u).create
                  UserTasks.setupRegistration(u).create
                }

                // TODO why the heck am i sleeping?
                //              Thread.sleep(1000)
                SendEmail.withSession(Website.realm(request)) { implicit mailSession =>
                  Tasks.sendEmailVerif(u)
                  if (!unameauto(u.yob))
                    Emailer.sendEmailUname(unameF(u.firstName, u.lastName), u)
                  Emailer.tellAdmin("New user", u.userName, u.emailDec, "realm: "+u.realms.mkString, "ABOUT: "+about)
                }
              }

              val stok = ROK.r

              //realm new user flow
              Services ! DieselMsgString(
                s"""$$msg rk.user.joined(userName="${u.userName}", realm="${stok.realm}")""",
                DieselTarget(
                  stok.realm,
                  WID.fromPath(s"${stok.realm}.Reactor:${stok.realm}#diesel").map(_.toSpecPath).toList,
                  Nil)
                )

              // process extra parms to determine what next
              request.session.get("extra") map { x =>
                val s = x split ","
                if (s.size > 0 && s(0).length > 0) {
                  Tasks.msgVerif(u, s"""\n<p><font style="color:red">Click below to continue joining the ${s(0)}.</font>""", Some("/wikie/linkuser/Club:" + s(0) + "?wc=0"))
                } else
                  Tasks.msgVerif(u, "", Some(routes.Profile.doeConsent().url))
              } getOrElse Tasks.msgVerif(u, "", Some(routes.Profile.doeConsent().url))
            }
          }) getOrElse {
            verror("ERR_CANT_UPDATE_USER.doeCreateProfile " + getFromSession("email", f + l + "@k.com"))
            unauthorized("Oops - cannot update this user [doeCreateProfile " + getFromSession("email", f + l + "@k.com") + "] - Please try again or send a suport request!").withNewSession
          }
      }) //fold
  }

  // user chose to login with google
  def doeJoinGtoken = RAction { implicit request =>
    val b = request.req.body.asFormUrlEncoded.get
    cdebug << "Google login request : " << b

    //todo better security - use random security token
    val g = try {
      (request.session.get("gaga").map(identity).getOrElse("1")).toLong
    } catch {
      case _: Throwable => 1
    }

    // allow this only for some minutes
    if (System.currentTimeMillis - g <= 120000) {
      val code = b("code").head
      val email = b("email").head
      val name = b("name").head
      val access_token = b("access_token").head
      val id = b("id").head

      // verifying the token
      val bres = Snakk.body(Snakk.url("https://www.googleapis.com/oauth2/v1/tokeninfo").form(Map("access_token"->access_token)))
      cdebug << "Google api response: " << bres
      val res = new JSONObject(bres)

      val CLIENT_ID="980280495626-7llkvk4o02anpu6qv1sseucc07f8f3gs.apps.googleusercontent.com"  // mine

      Audit.logdb("LOGIN_WITH_GOOGLE", email, name)

      if(id == res.getString("user_id") &&
        CLIENT_ID == res.getString("audience") ) {
        Users.findUserByEmailDec((email)) orElse (Users.findUserNoCase(email)) match {
          case Some(u) if (u.gid.exists(_.length > 0)) =>
            login(email, "", "", id)
          case Some(u) if (!u.gid.isDefined) =>
            Redirect(routes.Profile.doeJoin1Google).withSession("gaga" -> request.session.get("gaga").mkString, "extra" -> request.session.get("extra").mkString,  "gid" -> id)
          case None =>
            login(email, "", "", id)
        }
      } else {
        Audit.logdb("ERR_LOGIN_WITH_GOOGLE", "gaga expired")
        Unauthorized("")
      }
    } else {
      Msg2(
        """Session expired - please <a href="/doe/join">start again</a>.
          |<p>Please make sure your browser allows cookies for this website, thank you!""".stripMargin, Some("/doe/join")).withNewSession
    }
  }
}


object T {
  final val TESTCODE = "RazTesting"
}
