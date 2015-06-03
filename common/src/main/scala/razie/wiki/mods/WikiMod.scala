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
  /** modify the content of the page before formatting for rendering
    * @return None if no changed are desired*/
  def modPreFormat (we:WikiEntry, content:Option[String]) : Option[String]
  /** modify the resulting html of formatting the page
    * @return the new html */
  def modPostFormat (we:WikiEntry, html:String) : String
}

object WikiMods {//extends WikiDomain (Wikis.RK) {
  val mods = mutable.HashMap[String, WikiMod]()
  val index = mutable.HashMap[String, WikiMod]()

  def register (mod:WikiMod) = {
    mods.put(mod.modName, mod)
    mod.modProps.foreach(index.put(_, mod))
  }
}

