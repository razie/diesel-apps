/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.model

import com.mongodb.casbah.Imports._
import org.joda.time.DateTime
import razie.db._
import razie.wiki.Services

/** a link between two wikis */
@RTable
case class WikiLinkStaged(
  from: WID,
  to: WID,
  how: String,
  crDtm: DateTime = DateTime.now(),
  _id: ObjectId = new ObjectId()) extends REntity[WikiLinkStaged] {
}

/** a link between two wikis */
@RTable
case class WikiLink(
  from: UWID,
  to: UWID,
  how: String,
  draft:Option[String] = None,
  crDtm: DateTime = DateTime.now(),
  _id: ObjectId = new ObjectId()) extends REntity[WikiLink] {

  /** the name of the link page, if there will be any */
  val wname = Array(from.cat, from.nameOrId, to.cat, to.nameOrId).mkString(":")

  def page = Wikis(from.getRealm).find("WikiLink", wname)
  def pageFrom = Wikis(from.getRealm).find(from)
  def pageTo = Wikis(to.getRealm).find(to)

  def isPrivate = List(pageFrom, page).flatMap(_ map (_.isPrivate)).exists(identity)
}

/** most information about a page - represents the z End of an association, as you get it from the a End */
case class ILink(wid: WID, label: String, role:Option[String] = None, tags: Map[String, String] = Map(), ilinks: List[ILink] = Nil) {
  def this (wid:WID) = this(wid, wid.name)
  def href = "/wiki/%s".format(wid.wpath)
  def format = Wikis.formatWikiLink(Wikis.RK, wid, wid.name, label, None)
  def format (max:Int = -1) = Wikis.formatWikiLink(Wikis.RK, wid, wid.name, label, None, None, false, max)
}

