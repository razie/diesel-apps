package controllers

import com.google.inject._
import model.CMDWID
import play.api.Configuration
import razie.diesel.dom.RDOM.P
import razie.{Logging, XP, XpSolver}
import razie.wiki.model.{ILink, IWikiWrapper, WID, Wikis}
import razie.Debug._

/** support features */
@Singleton
class DieselAssets @Inject()(config:Configuration) extends RazController with Logging {

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

/* global */
object DieselAssets {

  def mkLink(w:WID, path:String) = {
    w.cat match {
      case "DieselEngine" => {
        val x = s"""diesel/viewAst/${w.name}"""
       x
      }
      case _ => s"""wiki/$path"""
    }
  }
}


