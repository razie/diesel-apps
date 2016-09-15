/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package controllers

import play.api.mvc.{Controller, _}
import razie.wiki.Services
import razie.{Logging}
import razie.wiki.model.WikiUser

/** common controller utilities */
class RazControllerBase extends Controller with Logging with Validation {

    /** authentication - find the user currently logged in */
  def wauth(implicit request: Request[_]): Option[WikiUser] = {
    val au = Services.auth.authUser (request)
//    au.foreach(u=> NoStaticS.put[WikiUser](u))
    au
  }

  /** result is a page to display this message and optionally continue to another page */
  def Msg2(msg: String, page: Option[String], u: Option[WikiUser] = None)(implicit request: Request[_]): play.api.mvc.Result = {
    ViewService.impl.utilMsg(msg, "", page, if (u.isDefined) u else wauth)
  }

  /** result a page to display this message and optionally continue to another page */
  def Msg3(msg: String, page: Option[String], pageNO:Option[(String,String)], u: Option[WikiUser] = None)(implicit request: Request[_]): play.api.mvc.Result = {
    ViewService.impl.utilMsg(msg, "", page, if (u.isDefined) u else wauth, pageNO)
  }
}

/** some views you have to provide for the flow */
trait ViewService {
  def utilMsg (msg:String, details:String, link:Option[String], user:Option[WikiUser], link2:Option[(String,String)]=None)(implicit request: RequestHeader): play.api.mvc.Result
}

object ViewService {
  var impl : ViewService = null
}
