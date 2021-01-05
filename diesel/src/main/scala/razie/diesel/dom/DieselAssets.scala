/** ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.dom

import razie.tconf.{FullSpecRef, TSpecRef}
import razie.wiki.model.WID
import scala.collection.mutable

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
  def mkLinkAsset(w: FullSpecRef, path: String = "") = {
    val x = s"""/diesel/objBrowserById/${w.inventory}/${w.conn}/${w.category}/${w.key}"""
    x
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
        val h = w.sourceUrl.mkString
        s"""<a href="$h/diesel/engine/view/${w.name}">${w.name}</a>"""
      }
      case _ => s"""wiki/$path"""
    }
  }

  // ================================

  /** find an element by ref */
  def findByRef(ref: FullSpecRef, collectRefs: Option[mutable.HashMap[String, String]] = None)
  : Option[DieselAsset[_]] = {
    val dom = WikiDomain(ref.realm)
    val p = dom.findPlugins(ref.inventory).headOption
    val o = p.flatMap(x => DomInventories.resolve(ref.realm, x.findByRef(dom.rdom, ref, collectRefs)))
    o
  }

  def listView(ref: TSpecRef, format: String = "default") = {

  }

  def view(ref: TSpecRef, format: String = "default") = {

  }
}
