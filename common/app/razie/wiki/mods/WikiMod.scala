/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.mods

import razie.wiki.model._

import scala.collection.mutable

/**
 * mods can mess with wikis. can add/interpret properties, filter html etc
  */
trait WikiMod {
  /** the name of the mod */
  def modName:String
  /** the wiki properties recognied by the mod */
  def modProps:Seq[String]
  /** is mod interested in topic */
  def isInterested (we:WikiEntry) : Boolean

  /** modify a prop */
  def modProp (prop:String, value:String, we:Option[WikiEntry]) : String

  /** modify the content of the page before parsing - you shouldn't really use this !
    * @return None if no changed are desired
    *
    * @param we the page in the context of which the transformation is required
    * @param content an optional different content. Implementations are supposed to start with
    *
    * content.orElse(Some(we.content)).map{c=>
    *
    * */
  def modPreParsing (we:WikiEntry, content:Option[String]) : Option[String] = None

  /** modify the content of the page before MARKDOWN formatting for rendering (but after internal formatting and parsing)
    * @return None if no changed are desired
    *
    * @param we the page in the context of which the transformation is required
    * @param content an optional different content. Implementations are supposed to start with
    *
    * content.orElse(Some(we.content)).map{c=>
    *
    * */
  def modPreHtml (we:WikiEntry, content:Option[String]) : Option[String] = None

  /** modify the resulting html of formatting the page
    * @return the new html */
  def modPostHtml (we:WikiEntry, html:String) : String = html
}

object WikiMods {//extends WikiDomain (Wikis.RK) {
  val mods = mutable.HashMap[String, WikiMod]()
  val index = mutable.HashMap[String, WikiMod]()

  def register (mod:WikiMod) = {
    mods.put(mod.modName, mod)
    mod.modProps.foreach(index.put(_, mod))
  }

  /** modify the content of the page before parsing - you shouldn't really use this !
    * @return None if no changed are desired
    *
    * @param we the page in the context of which the transformation is required
    * @param content an optional different content. Implementations are supposed to start with
    *
    * content.orElse(Some(we.content)).map{c=>
    *
    * */
  def modPreParsing (we:WikiEntry, content:Option[String]) : Option[String] =
    mods.values.foldLeft(content){(a,b)=>
      if(b.isInterested(we)) {
        we.cacheable = false
        b.modPreParsing(we, a)
      } else a
    }

  /** modify the content of the page before MARKDOWN formatting for rendering (but after internal formatting and parsing)
    * @return None if no changed are desired
    *
    * @param we the page in the context of which the transformation is required
    * @param content an optional different content. Implementations are supposed to start with
    *
    * content.orElse(Some(we.content)).map{c=>
    *
    * */
  def modPreHtml (we:WikiEntry, content:Option[String]) : Option[String] =
    mods.values.foldLeft(content){(a,b)=>
      if(b.isInterested(we)) {
        we.cacheable = false
        b.modPreHtml(we, a)
      } else a
    }

  /** modify the resulting html of formatting the page
    * @return the new html */
  def modPostHtml (we:WikiEntry, html:String) : String =
    mods.values.foldLeft(html) {(a,b)=>
      if(b.isInterested(we)) {
        we.cacheable = false
        b.modPostHtml(we, a)
      } else a
    }
}

