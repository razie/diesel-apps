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
import razie.audit.Audit
import razie.db._
import razie.db.RazSalatContext.ctx

import scala.collection.mutable.ListBuffer
import razie.db.tx.txn

/**
  * a thread / series of comments on something - like a forum topic
  *
  * threads apply to: wiki topics, PMs, MAs, questions, forums
  */
@RTable
case class CommentStream(
  topic: ObjectId, // for wiki, this is the WID or the parent record (MsgThread, ForumTopic etc)
  what: String = "Wiki", // indicates the type of parent Wiki/Msg
  crDtm: DateTime = DateTime.now(),
  _id: ObjectId = new ObjectId()) {

  def create = {
    RCreate(this)
    this
  }

  lazy val comments = new ListBuffer[Comment]() ++
    RMany.raw[Comment] ("streamId" -> _id).sort(Map("crDtm" -> 1)).map(grater[Comment].asObject(_)).toList

  def addComment(user: WikiUser, content: String, cid:String, link:Option[String], kind:Option[String], parentId: Option[ObjectId] = None) = {
    val c = Comment (_id, user._id, user.userName, parentId, content, link, kind, List.empty, List.empty, DateTime.now(), DateTime.now(), new ObjectId(cid))
    comments += c
    c create user
  }

  /** delete this thread and all comments */
  def delete = {
    Audit.logdb(Comments.AUDIT_COMMENT_UPDATED, "\nDELETED:\n")
    comments foreach (_.delete)
    RDelete.noAudit[CommentStream](this)
  }

  def isDuplo (oid:String) = comments.exists(_.id equals oid)
}

/**
 * a comment in a series
 *
 * todo add markdown options, bbcode vs md etc
 */
@RTable
case class Comment (
  streamId: ObjectId, // the stream this comment is part of
  userId: ObjectId, // user that made the comment
  userName: String, // user that made the comment
  parentId: Option[ObjectId], // in reply to...
  content: String,
  link: Option[String],
  kind: Option[String], // text/video/photo/resource
  likes: List[String]=List.empty, // list of usernames that liked it
  dislikes: List[String]=List.empty, // list of usernames that liked it
  crDtm: DateTime = DateTime.now(),
  updDtm: DateTime = DateTime.now(),
  _id: ObjectId = new ObjectId()) {

  def id = _id.toString

  def create (user:WikiUser) = {
    Audit.logdb(Comments.AUDIT_COMMENT_CREATED, "BY " + user.userName + " " + userId + " parent:" + parentId, "\nCONTENT:\n" + this)
    RCreate.noAudit[Comment](this)
  }

  //todo keep track of older versions and who modifies them - comment-history
  def like(like:Option[WikiUser], dislike:Option[WikiUser]) = {
    val u = this.copy (likes=like.map(_._id.toString).toList ::: likes, dislikes=dislike.map(_._id.toString).toList ::: dislikes)
//    Audit.logdb(Comments.AUDIT_COMMENT_UPDATED, "BY " + user.userName + " " + userId + " parent:" + parentId, "\nCONTENT:\n" + u)
    RUpdate.noAudit[Comment](Map("_id" -> _id), u)
    //todo moderation stream - send to a stream of content changes for bots / moderators
  }

  //todo keep track of older versions and who modifies them - comment-history
  def update(newContent: String, newLink:Option[String], user:WikiUser) = {
    val u = this.copy (content=newContent, link=newLink, updDtm=DateTime.now)
    Audit.logdb(Comments.AUDIT_COMMENT_UPDATED, "BY " + user.userName + " " + userId + " parent:" + parentId, "\nCONTENT:\n" + u)
    RUpdate.noAudit[Comment](Map("_id" -> _id), u)
    //todo moderation stream - send to a stream of content changes for bots / moderators
  }

  def delete = {
    Audit.logdb(Comments.AUDIT_COMMENT_UPDATED, "\nDELETED:\n")
    RDelete.noAudit[Comment](this)
    // todo if last, should also remove the comment stream? or maybe not
    //todo moderation stream - send to a stream of content changes for bots / moderators
  }

}

/** factory and utils */
object Comments {
  def findForWiki(id:ObjectId) = ROne[CommentStream]("what" -> "Wiki", "topic" -> id)
  def findFor(id:ObjectId, role:String) = ROne[CommentStream]("what" -> role, "topic" -> id)
  def findCommentById(id: String) = ROne[Comment](new ObjectId(id))

  final val AUDIT_COMMENT_CREATED = "COMMENT_CREATED"
  final val AUDIT_COMMENT_UPDATED = "COMMENT_UPDATED"
}
