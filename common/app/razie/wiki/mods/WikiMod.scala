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
 *
 * There is a mod wiki and a mod prop.
 * The modProp can tranform sections/tags while the modWiki transforms all wiki
  */
trait WikiMod {
  /** the name of the mod */
  def modName:String

  /** is mod interested in topic - you could look at tags, props or content
    *
    * note that for modProp you don't need to be interested
    */
  def isInterested (we:WikiEntry) : Boolean

  /** the wiki properties recognied by the mod - this is used by the parser and filtered through here.
    *
    * The idea is to allow you to define your own properties that you handle
    *
    * Note that for modProp you don't need to be interested
    */
  def modProps:Seq[String]

  /** modify a prop - during parsing, if you recognize this property, you can transform its value
    *
    * The idea is to allow you to define your own properties that you handle - i.e. a JSON formatter or
    * the on.snow modules for mod.book
    */
  def modProp (prop:String, value:String, we:Option[WikiEntry]) : String = value

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

  /** special actions available for this wiki?
    *
    * main actions are "edit", "view" - which you could overwrite
    *
    * @return more special actions, map(name,lavel,URL)
    */
  def modActions (we:WikiEntry) : List[(String,String,String)] = List.empty
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

  /** special actions available for this wiki?
    *
    * @return more special actions, map(name,label,URL)
    */
  def modActions (we:WikiEntry) : List[(String,String,String)] =
    mods.values.foldLeft(List.empty[(String,String,String)]) {(a,b)=>
      if(b.isInterested(we)) {
        we.cacheable = false
        val ac = b.modActions(we)
        ac ::: a.filterNot(tt=>ac.exists(_._1 == tt._1))
      } else a
    }

}

