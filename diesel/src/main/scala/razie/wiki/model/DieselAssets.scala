/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package controllers

import razie.wiki.model.WID

/* global */
object DieselAssets {

  /** make a link to see the asset, embedded elsewhere, simplified view (no footers etc) */
  def mkEmbedLink(w: WID, path: String = "") = {
    w.cat match {
      case "DieselEngine" => {
        val x = s"""diesel/viewAst/${w.name}"""
        x
      }
      case _ => s"""wiki/$path"""
    }
  }

  /** make a link to see the asset */
  def mkLink(w: WID, path: String = "") = {
    w.cat match {
      case "DieselEngine" => {
        val x = s"""diesel/viewAst/${w.name}"""
        x
      }
      case _ => s"""wiki/$path"""
    }
  }

  def mkEditLink(w: WID, path: String = "") = {
    w.cat match {
      case "DieselEngine" => {
        val x = s"""diesel/viewAst/${w.name}"""
        x
      }
      case _ => s"""wikie/editold/$path"""
    }
  }

  /** make a link to see the asset */
  def mkAhref(w: WID, path: String = "") = {
    w.cat match {
      case "DieselEngine" => {
        // todo what's diff /diesel/engine/view vs /diesel/viewAst
        s"""<a href="/diesel/engine/view/${w.name}">${w.name}</a>"""
      }
      case _ => s"""wiki/$path"""
    }
  }

}
