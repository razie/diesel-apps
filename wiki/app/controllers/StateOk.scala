package controllers

import model._
import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import play.api.mvc._
import play.twirl.api.Html
import razie.wiki.model._
import razie.wiki.Services

import scala.collection.mutable

// todo refactor
object Res extends Results

/** captures the current state of what to display - passed to all views */
class StateOk(val realm:String, val au: Option[model.User], val request: Option[Request[_]]) {
  var msg: Seq[(String, String)] = Seq.empty
  var _title : String = "" // this is set by the body as it builds itself and used by the header, heh
  val _metas = new mutable.HashMap[String,String]() // moremetas
  var _css : Option[String] = None // if you determine you want a different CSS
  var stuff : List[String] = List() // keep temp status for this request
  var bottomAd : Boolean = false

  def showBottomAd(yes:Boolean) = {bottomAd = yes}

  lazy val query = request.map(_.queryString.map(t=>(t._1, t._2.mkString))).getOrElse(Map.empty)
  def formParms = request.flatMap(_.asInstanceOf[Request[AnyContent]].body.asFormUrlEncoded.map(_.collect { case (k, v) => (k, v.head) })).get
  def formParm(name:String) = request.map(_.asInstanceOf[Request[AnyContent]].body.asFormUrlEncoded.get.get(name).map(_.mkString).mkString).mkString

  /** set the title of this page */
  def title(s:String) = {
    this._title = s;
  }

  /** messages are "msg" -> "blah" or "ok" -> "blah" or "err" -> "blah" */
  def msg (imsg: Seq[(String, String)]) : StateOk = {
    msg = imsg
    this
  }

  /** messages are "msg" -> "blah" or "ok" -> "blah" or "err" -> "blah" */
  def msg (imsg: (String, String)) : StateOk = {
    msg = Seq(imsg)
    this
  }

  /** set the title of this page */
  def css(s:String) = {this._css = Some(s); ""}
  def maybeCss(s:Option[String]) = {this._css = s; ""}
  def css = {
    val ret = _css.getOrElse {
      // session settings override everything
      request.flatMap(_.session.get("css")) orElse (
        // then user
        au.flatMap(_.css)
        ) orElse (
        // or website settings
        request.flatMap(r=> Website(r)).flatMap(_.css)
        ) getOrElse ("light")
    }

    if(ret == null || ret.length <=0) "light" else ret
  }
  def isLight = css contains "light"

  /** add a meta to this page's header */
  def meta(name:String, content:String) = {this._metas.put(name, content); ""}
  def metas = _metas.toMap

//  def stokRequest (implicit stok:StateOk) = {
//    val r = new RazRequest(stok.request.get)
//    r.msg = stok.msg
//    r._title = stok._title
//    r._css = stok._css
//    r
//  }
  def reactorLayout12 (content: StateOk => Html) =
    Res.Ok (views.html.util.reactorLayout12(content(this), msg)(this))

  def apply (content: => Html) =
    Res.Ok (views.html.util.reactorLayout(content, msg)(this))

  def apply (content: StateOk => Html) =
    Res.Ok (views.html.util.reactorLayout(content(this), msg)(this))

  def justLayout (content: StateOk => Html) =
    views.html.util.reactorLayout(content(this), msg)(this)

  def notFound (content: StateOk => Html) =
    Res.NotFound (views.html.util.reactorLayout(content(this), msg)(this))

  /** use when handling forms */
//  def badRequest (content: => Html) =
//    RkViewService.BadRequest (views.html.util.reactorLayout(content, msg)(this))

  def badRequest (content: StateOk => Html) = {
    msg("err" -> "[Form has errors]")
    Res.BadRequest (views.html.util.reactorLayout12(content(this), msg)(this))
  }

  def badRequest (content: => Html) = {
    msg("err" -> "[Form has errors]")
    Res.BadRequest (views.html.util.reactorLayout12(content, msg)(this))
  }

  /** use for old templates with embedded layout OR plaint text */
  def noLayout (content: StateOk => Html) =
    Res.Ok (content(this))

  def website = Website.gets(this)
}

/** trying some type foolery - pass this off as a Request[_] as well and proxy to original */
class RazRequest (realm:String, au:Option[User], val ireq:Request[_]) extends StateOk (
  realm,
  au orElse Services.auth.authUser(ireq).asInstanceOf[Option[User]],
  Some(ireq))
//  with play.api.mvc.RequestHeader
{

  def this (ireq:Request[_]) = this(
    Website.getRealm (ireq),
    None,
    ireq
  )

  def req = ireq.asInstanceOf[Request[AnyContent]]
  def oreq = Some(ireq)
  //  lazy val au =

  lazy val stok = this //new controllers.StateOk(Seq(), realm, au, Some(request))
  lazy val errCollector = new VErrors()

  def session = ireq.session
  def flash = ireq.flash

  def id : scala.Long = ireq.id
  def tags : scala.Predef.Map[scala.Predef.String, scala.Predef.String] = ireq.tags
  def headers : play.api.mvc.Headers = ireq.headers
  def queryString : scala.Predef.Map[scala.Predef.String, scala.Seq[scala.Predef.String]] = ireq.queryString
  def path : scala.Predef.String = ireq.path
  def uri : scala.Predef.String = ireq.uri
  def method : scala.Predef.String = ireq.method
  def version : scala.Predef.String = ireq.version
  def remoteAddress : scala.Predef.String = ireq.remoteAddress
  def secure : scala.Boolean = ireq.secure

//  override def id : scala.Long = ireq.id
//  override def tags : scala.Predef.Map[scala.Predef.String, scala.Predef.String] = ireq.tags
//  override def headers : play.api.mvc.Headers = ireq.headers
//  override def queryString : scala.Predef.Map[scala.Predef.String, scala.Seq[scala.Predef.String]] = ireq.queryString
//  override def path : scala.Predef.String = ireq.path
//  override def uri : scala.Predef.String = ireq.uri
//  override def method : scala.Predef.String = ireq.method
//  override def version : scala.Predef.String = ireq.version
//  override def remoteAddress : scala.Predef.String = ireq.remoteAddress
//  override def secure : scala.Boolean = ireq.secure
}

// todo I can't use this - when using it, too many errors from implicits and stuff
class BetterRazRequest[A] (val request:Request[A]) extends WrappedRequest[A] (request) {
  def oreq = Some(request)
  lazy val au = Services.auth.authUser(request).asInstanceOf[Option[User]]
  lazy val realm = Website.getRealm (request)
  lazy val stok = new controllers.StateOk(realm, au, Some(request))
  lazy val errCollector = new VErrors()
}


