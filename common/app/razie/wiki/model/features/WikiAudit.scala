/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.model

import com.novus.salat.annotations._
import org.bson.types.ObjectId
import org.joda.time.DateTime
import razie.db.{RCreate, RTable, Txn, tx}

/**
 * auditing events on wiki pages, like edits, views etc
 *
 * These backed up in their own table
 *
 * These are ALSO used CQRS style with WikiObserver - you can listen there
 *
 * TODO should probably allow customization to track only interesting events, i.e. filters
 *
 * for now - just purge the table as you feel like...
 */
@RTable
case class WikiAudit(
  event: String,
  wpath: String,
  userId: Option[ObjectId],
  details: Option[String] = None, // extra details
  @Ignore page:Option[WikiEntry] = None, // always the page this is about, after any updates
  @Ignore oldPage:Option[WikiEntry] = None, // in case of update, the old page
  oldWpath:Option[String] = None, // in case of update, the old page
  node:Option[String] = None,
  crDtm: DateTime = DateTime.now,
  _id: ObjectId = new ObjectId()) {

  def create(implicit txn: Txn = tx.auto) = RCreate noAudit this

  def toEvent = WikiEvent(
    event,
    "WikiEntry",
    page.map(_.wid.wpathFull).getOrElse(wpath),
    page,
    oldPage,
    oldPage.map(_.wid.wpathFull).orElse(oldWpath),
    node.mkString
  )
}

/** audit codes */
object WikiAudit {
  final val CREATE_WIKI = "create.UPD_CREATE"
  final val CREATE_API = "create.SET_CONTENT"

  final val MOVE_POSTS = "update.MOVE_POSTS"
  final val UPD_PARENT = "update.UPD_PARENT"
  final val UPD_SETP_PARENT = "update.UPD_SETP_PARENT"
  final val UPD_SET_CONTENT = "update.UPD_SET_CONTENT"
  final val UPD_CONTENT = "update.UPD_CONTENT"
  final val UPD_TOGGLE_RESERVED = "update.UPD_TOGGLE_RESERVED"
  final val UPD_UOWNER = "update.UPD_UOWNER"
  final val UPD_CATEGORY = "update.UPD_CATEGORY"
  final val UPD_EDIT = "update.UPD_EDIT"
  final val UPD_REALM = "update.UPD_REALM"
  final val UPD_RENAME = "update.RENAME"
  final val UPD_LIKE = "update.LIKE"

  final val DELETE_WIKI = "delete.wiki"

  def isUpd(s:String) = s.startsWith("create.") || s.startsWith("update.") || s.startsWith("delete.")
}

