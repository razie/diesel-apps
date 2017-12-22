package controllers

import mod.msg.MsgThread
import mod.snow.RacerKidz
import model.{User, Users}
import org.bson.types.ObjectId
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc.Action
import razie.Logging
import razie.db.{ROne, tx}
import razie.wiki.admin.SendEmail
import razie.wiki.model._

object Comment extends RazController with Logging {
  val commentForm = Form {
    tuple(
      "link" -> text.verifying(vSpec, vBadWords),
      "content" -> text.verifying(vSpec, vBadWords))
  }

  /** add a comment **/
  def add(topicId: String, role:String, what: String, oid: String) = FAU {
    implicit au => implicit errCollector => implicit request =>

    commentForm.bindFromRequest.fold(
      formWithErrors => //todo fix realm
        Msg2(formWithErrors.toString + "Hein?", Some(routes.Wiki.showId(topicId).url)),
      {
        case (link, content) => iadd(topicId, role, what, oid, None, content).apply(request).value.get.get
      })
  }

  def kopt (kind:String) = if(Array ("video","photo","slideshow") contains kind) Some(kind) else None

  /** add a comment **/
  private def iadd(pid: String, role:String, kind: String, cid: String, link:Option[String], content: String) = FAU {
    implicit au => implicit errCollector => implicit request =>

    if(checkEntity(pid, role)) {
      val cs = Comments.findFor(new ObjectId(pid), role).getOrElse(CommentStream(new ObjectId(pid), role).create)
      if (!cs.isDuplo(cid)) {
        cs.addComment(au, content, cid, link, kopt(kind))
        // TODO send email to parent if kid
        notifyForEntity (pid, role, au, cs)
      } else log("ERR_DUPLO_COMMENT")
      redirect(pid, role)
    } else {
      Unauthorized("Oops - cannot add comment... " + errCollector.mkString)
    }
  }

  def reply(topicId: String, what: String, replyId: String, oid: String) = FAU {
    implicit au => implicit errCollector => implicit request =>

    commentForm.bindFromRequest.fold(
      formWithErrors =>
        Msg2(formWithErrors.toString + "Hein?", Some("/wiki/id/" + topicId)),
      {
        case (link, content) =>
          (for (
            cs <- Comments.findForWiki(new ObjectId(topicId)) orErr ("No comments for this page...");
            parent <- cs.comments.find(_._id.toString == replyId) orErr ("Can't find the comment to reply to...")
          ) yield {
            cs.addComment(au, content, oid, None, None, Some(parent._id))
            Redirect(routes.Wiki.showId(topicId)) //todo fix realm
          }) getOrElse {
            Unauthorized("Oops - cannot add comment... " + errCollector.mkString)
          }
      })
  }

  /** is mine or i am amdin */
  def canEdit(comm: Comment, auth: Option[User]) = {
    auth.exists(au => comm.userId == au._id || au.hasPerm(Perm.adminDb))
  }

  /** is mine or i am amdin */
  def canRemove(comm: Comment, auth: Option[User]) = {
    auth.exists(_.hasPerm(Perm.adminDb))
  }

  /** split a comment back into a photo/video etc */
  private def split(content: String, kind: String) = {
    if (content startsWith ("{{" + kind)) {
      val pat = ("(?s)\\{\\{"+kind+" ([^}]*)\\}\\}(.*)").r
      val pat(l,c) = content
      (l,c)
    } else
      ("", content)
  }

  def edit(pid: String, role:String, cid: String) = FAU {
    implicit au => implicit errCollector => implicit request =>

      (for (
        comm <- Comments.findCommentById(cid) orErr ("bad comment id?");
        can <- canEdit(comm, auth) orErr ("can only edit your comments")
      ) yield {
          val (link, content) = split (comm.content, comm.kind.getOrElse("text"))
          ROK.r noLayout { implicit stok =>
            views.html.comments.commEdit(pid, role, cid, comm.kind.getOrElse("text"), commentForm.fill(link, content.trim))
          }
        }) getOrElse
        unauthorized()
  }

  def like(pid: String, role:String, cid: String, yes:Int) = FAU {
    implicit au => implicit errCollector => implicit request =>

      (for (
        comm <- Comments.findCommentById(cid) orErr ("bad comment id?")
      ) yield {
          comm.like((if(yes==1) Some(au) else None), (if(yes==0)Some(au) else None))
          redirect(pid, role)
        }) getOrElse
        unauthorized()
  }

  def remove(pid: String, role:String, cid: String) = FAU {
    implicit au => implicit errCollector => implicit request =>

    (for (
      comm <- Comments.findCommentById(cid) orErr ("bad comment id?");
      can <- canRemove(comm, auth) orErr ("Only admins can remove comments")
    ) yield {
        comm.delete
        redirect(pid, role)
    }) getOrElse
      unauthorized()
  }

  def save(pid: String, role:String, cid: String, kind: String) = FAU {
    implicit au => implicit errCollector => implicit request =>

    commentForm.bindFromRequest.fold(
      formWithErrors => {
        log(formWithErrors.toString)
        BadRequest(
          ROK.r justLayout { implicit stok =>
            views.html.comments.commEdit(pid, role, cid, kind, formWithErrors)
          }
        )
      },
      {
        case (newlink, newcontent) =>
          val con = if (newlink.length <= 0) newcontent else {
          (kind match {
            case "video" =>
              if ((newlink contains "<a href") || (newlink contains "<iframe")) newlink
              else "{{video " + newlink + "}}"
            case "photo" =>
              if ((newlink contains "<a href") || (newlink contains "<img")) newlink
              else "{{photo " + newlink + "}}"
            case "slideshow" =>
              if ((newlink contains "<a href") || (newlink contains "<iframe") || (newlink contains "<embed")) newlink
              else "{{slideshow " + newlink + "}}"
            case _ => ""
          }) + (if(newcontent.trim.length > 0) ("\n\n"+newcontent.trim) else "")
          }

          Comments.findCommentById(cid) map { comm =>
            (for (
              can <- canEdit(comm, auth) orErr ("can only edit your comments")
            ) yield {
              if (con.length > 0)
                comm.update(con, None, au)
              redirect(pid, role)
            }) getOrElse
              unauthorized()
          } getOrElse {
            if (con.length > 0)
              iadd(pid, role, kind, cid, kopt(newlink), con).apply(request).value.get.get
            else
              redirect(pid, role)
          }
      })
  }

  // todo this entire can be async
  def notifyForEntity (pid:String, role:String, au:User, cs:CommentStream) = {
    SendEmail.withSession() { implicit mailSession =>
    if("Wiki" == role) {
        // email creator and all other commenters
        Wikis.findById(pid).map { w =>
          au.shouldEmailParent("Everything").map{parent =>
            Emailer.sendEmailChildCommentWiki(parent, au, w.wid)
          }
          (
            // topic owner and all that commented on it
            Users.findUserById(w.by).map(_._id).toList ++
              cs.comments.map(_.userId)
          ).distinct.filter(_ != au._id).map {uid =>
            Users.findUserById(uid).filter(_.isActive).map {u =>
              Emailer.sendEmailNewComment(u, au, w.wid)
              RacerKidz.myself(u._id).history.post(w, au, Some("New comment posted by "+au.fullName))
            }
          }
        }
    } else if("PM" == role || "Msg" == role || "MA" == role)
      ROne[MsgThread](new ObjectId(pid)).map {msg=>
        var newMsg = msg
        // will email only once, not every time comment is added - also whenteh first comment is added
        msg.users.filter(x=> x.userId != au._id && (x.read || cs.comments.size == 1)).map(_.userId).distinct.flatMap(uid=>Users.findUserById(uid)).filter(_.isActive).foreach {user=>
          Emailer.sendEmailNewComment(user, au, WID("Admin", "Private Messages"))
          newMsg = msg.readNow(user._id, false)
        }
        newMsg.update(tx.auto)
      }
    }
  }

  /** redirect to the parent entity */
  def redirect (pid:String, role:String) = {
    if("Wiki" == role)
      Redirect(controllers.Wiki.w(Wikis.find(new ObjectId(pid)).get.wid, false))
    else if("PM" == role || "Msg" == role)
      Redirect(mod.msg.routes.Messaging.doeThread(pid))
    else
      Redirect("/")
  }

  /** validate the parent entity */
  def checkEntity (pid:String, role:String) = {
    (role == "Wiki" && Wikis.findById(pid).isDefined) ||
      ((role == "PM" || role == "Msg" || role == "MA") && ROne[MsgThread](new ObjectId(pid)).isDefined)
  }

  /** start to add a video/photo comment **/
  def startComment(pid: String, role:String, oid: String, kind: String) = FAU {
    implicit au => implicit errCollector => implicit request =>

    if(checkEntity(pid, role)) {
        ROK.r apply { implicit stok =>
          views.html.comments.commEdit(pid, role, oid, kind, commentForm.fill("", ""))
        }
    } else
      unauthorized("Wiki topic not found")
  }
}

