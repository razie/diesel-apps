/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.model

import com.mongodb.casbah.Imports._
import com.novus.salat.grater
import org.bson.types.ObjectId
import org.joda.time.DateTime
import razie.db.{RCreate, RMany, ROne, RTable, RUpdate}
import razie.db.RazSalatContext.ctx
import razie.wiki.admin.Audit

import scala.collection.mutable.ListBuffer

/** a series of comments on something - like a forum topic */
@RTable
case class CommentStream(
  //todo there are a few records with WID instead of ID here...
  //todo this needs to become UWID - migrate RK
  topic: ObjectId, // for wiki, this is the WID
  what: String = "Wiki", // "wiki" vs ? if i ever turn this into somthing more complicated
  crDtm: DateTime = DateTime.now(),
  _id: ObjectId = new ObjectId()) {

  def create = {
    RCreate(this)
    this
  }

  lazy val comments = new ListBuffer[Comment]() ++
    RMany.raw[Comment] ("streamId" -> _id).sort(Map("crDtm" -> 1)).map(grater[Comment].asObject(_)).toList

  def addComment(user: WikiUser, content: String, oid:String, link:Option[String], kind:Option[String], parentId: Option[ObjectId] = None) = {
    val c = Comment (_id, user._id, parentId, content, link, kind, DateTime.now(), DateTime.now(), new ObjectId(oid))
    comments += c
    c create user
  }

  def isDuplo (oid:String) = comments.exists(_.id equals oid)
}

/** a series of comments on something - like a forum topic 
 *  
 * todo add markdown options, bbcode vs md etc 
 */
@RTable
case class Comment(
  streamId: ObjectId, // the stream this comment is part of
  userId: ObjectId, // user that made the comment
  parentId: Option[ObjectId], // in reply to...
  content: String,
  link: Option[String],
  kind: Option[String], // text/video/photo
  crDtm: DateTime = DateTime.now(),
  updDtm: DateTime = DateTime.now(),
  _id: ObjectId = new ObjectId()) {

  def id = _id.toString

  def create (user:WikiUser) = {
    Audit.logdb(Comments.AUDIT_COMMENT_CREATED, "BY " + user.userName + " " + userId + " parent:" + parentId, "\nCONTENT:\n" + this)
    RCreate.noAudit[Comment](this)
  }

  //todo keep track of older versions and who modifies them - comment-history
  def update(newContent: String, newLink:Option[String], user:WikiUser) = {
    val u = new Comment(streamId, userId, parentId, newContent, newLink, this.kind, crDtm, DateTime.now, _id)
    Audit.logdb(Comments.AUDIT_COMMENT_UPDATED, "BY " + user.userName + " " + userId + " parent:" + parentId, "\nCONTENT:\n" + u)
    RUpdate.noAudit[Comment](Map("_id" -> _id), u)
  }
}

/** factory and utils */
object Comments {
  def findForWiki(id:ObjectId) = ROne[CommentStream]("what" -> "Wiki", "topic" -> id)
  def findById(id: String) = ROne[CommentStream](new ObjectId(id))
  def findCommentById(id: String) = ROne[Comment](new ObjectId(id))

  final val AUDIT_COMMENT_CREATED = "COMMENT_CREATED"
  final val AUDIT_COMMENT_UPDATED = "COMMENT_UPDATED"
}
