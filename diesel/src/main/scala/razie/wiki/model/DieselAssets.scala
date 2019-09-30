/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package controllers

import razie.wiki.model.WID

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
