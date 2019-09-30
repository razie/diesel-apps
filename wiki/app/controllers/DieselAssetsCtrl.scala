package controllers

import com.google.inject._
import model.CMDWID
import play.api.Configuration
import razie.Logging
import razie.wiki.model.WID

/** support features */
@Singleton
class DieselAssetsCtrl @Inject()(config:Configuration) extends RazController with Logging {

  // display the form
  def doeById(kind: String, id: String) = RAction { implicit request =>
    Redirect(WID(kind,id).urlRelative)
  }

  def doeByWid(wid: CMDWID) = RAction { implicit request =>
      wid.wid.map {w=>
        Redirect(w.urlRelative)
      }.getOrElse (
        Unauthorized("can't parse wid... ")
      )
  }

}

