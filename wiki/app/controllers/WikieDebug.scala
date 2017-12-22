/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package controllers

import razie.wiki.model.features.WikiCount
import com.google.inject.{Inject, Singleton}
import org.joda.time.DateTime
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms.nonEmptyText
import razie.db.{RMany, ROne}
import razie.diesel.dom.WikiDomain
import razie.wiki.Services
import razie.wiki.model._

/** wiki edits controller */
@Singleton
class WikieDebug @Inject() (config:Configuration) extends WikieBase {

  def manage(id: String) = FAUR("manage.wiki") { implicit request =>
    Wikis(getRealm()).findById(id).map{ w=>
      ROK.k noLayout { implicit stok =>
        views.html.wiki.wikieManage(Some(w))
      }
    }
  }

  /** move the posts of another blof to another or just one post if this is it */
  def movePosts(sourceWid: WID) = FAUR { implicit request =>
      val form = Form("newWid" -> nonEmptyText)

      form.bindFromRequest.fold(
      formWithErrors =>
        Msg2(formWithErrors.toString + "Oops - the form is errored out, man!"),
      {
        case newWid =>
          log("Wiki.movePosts " + sourceWid + ", " + newWid)
          (for (
            au <- request.au;
            ok1 <- au.hasPerm(Perm.adminDb) orCorr cNoPermission;
            sourceW <- Wikis.find(sourceWid);
            destWid <- WID.fromPath(newWid) orErr "no source";
            destW <- Wikis.find(destWid) orErr "no destination";
//            isFromPost <- ArWikiDomain.aEnds(sourceW.wid.cat, "Child").contains("Post") orErr "source has no child Posts/Items";
//            isToPost <- WikiDomain.aEnds(destW.wid.cat, "Child").contains("Post") orErr "dest has no child Posts/Items"
//            upd <- before(newVer, WikiEntry.UPD_UOWNER) orErr ("Not allowerd")
            nochange <- (sourceW.wid != destW.wid) orErr "no change"
          ) yield {
//            val links = RMany[WikiLink]("to" -> sourceW.uwid.grated, "how" -> "Child", "from.cat" -> "Post").toList
//            val pages = RMany[WikiEntry]("parent" -> Some(sourceW.uwid.id), "category"->"Post").toList

            val links =
              if(sourceWid.cat == "Post")
                ROne[WikiLink]("from.id" -> sourceW.uwid.id, "how" -> "Child").orElse (
                  Some(WikiLink(sourceW.uwid, destW.uwid, "Child")).map(x=>{x.create; x})).toList
              else
                RMany[WikiLink]("to.id" -> sourceW.uwid.id, "how" -> "Child").toList

            val pages =
              if(sourceWid.cat == "Post")
                List(sourceW)
              else
                RMany[WikiEntry]("parent" -> Some(sourceW.uwid.id)).toList
            razie.db.tx("Wiki.movePosts", au.userName) { implicit txn =>
              links.foreach { _.copy(to = destW.uwid).update }
              pages.foreach{w=>
                w.update(w.copy(parent=Some(destW.uwid.id)), Some("moved_posts"))
                Services ! WikiAudit(WikiAudit.UPD_PARENT, sourceW.wid.wpathFull, Some(au._id))
              }
            }
            val m = s" ${links.size} WikiLinks and ${pages.size} WikiEntry /posts from ${sourceW.wid.wpath} to ${destW.wid.wpath}"
            Services ! WikiAudit(WikiAudit.MOVE_POSTS, sourceW.wid.wpathFull, Some(au._id), Some(m))
            Msg2(s"Moved $m", Some(controllers.Wiki.w(sourceWid)))
          }) getOrElse
            noPerm(sourceWid, "ADMIN_MOVEPOSTS")
      })
  }

  /** update parent */
  def setParent(sourceWid: WID) = FAUR {implicit request =>

      val form = Form("newWid" -> nonEmptyText)

      form.bindFromRequest.fold(
      formWithErrors =>
        Msg2(formWithErrors.toString + "Oops, can't add that quota!"),
      {
        case newWid if newWid == "n/a" =>
          log("Wiki.setParent REMOVE" + sourceWid + ", " + newWid)
          (for (
            au <- request.au;
            ok1 <- au.hasPerm(Perm.adminDb) orCorr cNoPermission;
            sourceW <- Wikis.find(sourceWid);
            newVer <- Some(sourceW.copy(parent=None));
            upd <- before(newVer, WikiAudit.UPD_SETP_PARENT) orErr ("Not allowerd")
          ) yield {
              razie.db.tx("Wiki.setparent", au.userName) { implicit txn =>
                sourceW.update(newVer, Some("setParent"))
                ROne[WikiLink]("from.id" -> sourceW.uwid.id, "how" -> "Child").foreach(_.delete)
              }
              Services ! WikiAudit(WikiAudit.UPD_SETP_PARENT, sourceW.wid.wpathFull, Some(au._id), Some(newWid.toString))
              Redirect(controllers.Wiki.w(sourceWid))
            }) getOrElse
            noPerm(sourceWid, "ADMIN_MOVEPOSTS")
        case newWid =>
          log("Wiki.setParent " + sourceWid + ", " + newWid)
          (for (
            au <- activeUser;
            ok1 <- au.hasPerm(Perm.adminDb) orCorr cNoPermission;
            sourceW <- Wikis.find(sourceWid);
            destWid <- WID.fromPath(newWid) orErr "no dest found";
            destW <- Wikis.find(destWid) orErr "no destination";
            //            isFromPost <- ArWikiDomain.aEnds(sourceW.wid.cat, "Child").contains("Post") orErr "source has no child Posts/Items";
            //            isToPost <- WikiDomain.aEnds(destW.wid.cat, "Child").contains("Post") orErr "dest has no child Posts/Items"
            newVer <- Some(sourceW.copy(parent=Some(destW._id)));
            upd <- before(newVer, WikiAudit.UPD_SETP_PARENT) orErr ("Not allowerd")
          ) yield {
            razie.db.tx("Wiki.setparent", au.userName) { implicit txn =>
              sourceW.update(newVer, Some("setParent"))
              val owl = ROne[WikiLink]("from.id" -> sourceW.uwid.id, "how" -> "Child")
              owl.foreach(_.delete)
              WikiLink(
                sourceW.uwid,
                destW.uwid,
                "Child",
                owl.flatMap(_.draft),
                owl.map(_.crDtm).getOrElse(DateTime.now())
              ).create
            }
            Services ! WikiAudit(WikiAudit.UPD_SETP_PARENT, sourceW.wid.wpathFull, Some(au._id), Some(newWid.toString))
            Redirect(controllers.Wiki.w(sourceWid))
          }) getOrElse
            noPerm(sourceWid, "ADMIN_MOVEPOSTS")
      })
  }

  /** change owner */
  def update(what:String, wid: WID) = FAUR {implicit request =>
    val au = request.au.get

    val uownerForm = Form("newvalue" -> nonEmptyText)

    uownerForm.bindFromRequest.fold(
    formWithErrors =>
      Msg2(formWithErrors.toString + "Oops, can't add that quota!"),
    {
      case newvalue =>
        what match {
          case "owner" => {
            log("Wiki.uowner " + wid + ", " + newvalue)
            (for (
              ok1 <- au.hasPerm(Perm.adminDb) orCorr cNoPermission;
              w <- Wikis.find(wid);
              newu <- WikiUsers.impl.findUserByUsername(newvalue) orErr (newvalue + " User not found");
              nochange <- (!w.owner.exists(_.userName == newvalue)) orErr "no change";
              newVer <- Some(w.cloneProps(w.props + ("owner" -> newu._id.toString), au._id));
              upd <- before(newVer, WikiAudit.UPD_UOWNER) orErr "Not allowerd"
            ) yield {
              // can only change label of links OR if the formatted name doesn't change
              razie.db.tx("Wiki.uowner", au.userName) { implicit txn =>
                w.update(newVer)
              }
              Wikie.after(newVer, WikiAudit.UPD_UOWNER, Some(au))
              Redirect(controllers.Wiki.w(wid))
            }) getOrElse
              noPerm(wid, "ADMIN_UOWNER")
          }
          case "category" => {
            log("Wiki.category " + wid + ", " + newvalue)
            (for (
              ok1 <- au.hasPerm(Perm.adminDb) orCorr cNoPermission;
              w <- Wikis.find(wid);
              nochange <- (w.category != newvalue) orErr "no change";
              newVer <- Some(w.copy(category=newvalue, ver = w.ver+1));
              upd <- before(newVer, WikiAudit.UPD_UOWNER) orErr "Not allowerd"
            ) yield {
                // can only change label of links OR if the formatted name doesn't change
                razie.db.tx("Wiki.ucategory", au.userName) { implicit txn =>
                  w.update(newVer)
                }
                Wikie.after(newVer, WikiAudit.UPD_CATEGORY, Some(au))
                Redirect(controllers.Wiki.w(newVer.wid))
              }) getOrElse
              noPerm(wid, "ADMIN_UCATEGORY")
          }
          case "counter" => {
            log("Wiki.counter " + wid + ", " + newvalue)
            (for (
              ok1 <- au.hasPerm(Perm.adminDb) orCorr cNoPermission;
              w <- Wikis.find(wid)
            ) yield {
                // can only change label of links OR if the formatted name doesn't change
                razie.db.tx("Wiki.counter", au.userName) { implicit txn =>
                  WikiCount.findOne(w._id).foreach(_.set(newvalue.toLong))
                }
                Redirect(controllers.Wiki.w(wid))
              }) getOrElse
              noPerm(wid, "ADMIN_UOWNER")
          }
          case "realm" => {
            log("Wiki.urealm " + wid + ", " + newvalue)
            (for (
              ok1 <- au.hasPerm(Perm.adminDb) orCorr cNoPermission;
              w <- Wikis.find(wid);
              nochange <- (w.realm != newvalue) orErr "no change";
              newVer <- Some(w.copy(realm=newvalue, ver = w.ver+1));
              upd <- before(newVer, WikiAudit.UPD_UOWNER) orErr "Not allowerd"
            ) yield {
              // can only change label of links OR if the formatted name doesn't change
              razie.db.tx("Wiki.urealm", au.userName) { implicit txn =>
                w.update(newVer)
              }
              Wikie.after(newVer, WikiAudit.UPD_REALM, Some(au))
              Redirect(controllers.Wiki.w(wid))
            }) getOrElse
              noPerm(wid, "ADMIN_UOWNER")
          }
          case "realmALL" => {
            log("Wiki.urealmALL " + wid + ", " + newvalue)
            (for (
              ok1 <- au.hasPerm(Perm.adminDb) orCorr cNoPermission;
              w <- Wikis.find(wid);
//              nochange <- (w.realm != newvalue) orErr "no change";  // don't check nochange
              newVer <- Some(w.copy(realm=newvalue, ver = w.ver+1));
              upd <- before(newVer, WikiAudit.UPD_UOWNER) orErr "Not allowerd"
            ) yield {
              // can only change label of links OR if the formatted name doesn't change
              razie.db.tx("Wiki.urealmALL", au.userName) { implicit txn =>
                w.update(newVer)
                RMany[WikiLink]("to.id" -> w.uwid.id, "how"->"Child").toList.foreach{ link=>
                  link.delete
                  link.pageFrom.map{p=>
                    val c = p.copy(realm=newvalue, ver = p.ver+1)
                    p.update(c)
                    link.copy(to = newVer.uwid, from = c.uwid).create
                  } getOrElse {
                    link.copy(to = newVer.uwid).create
                  }
                }
              }
              Wikie.after(newVer, WikiAudit.UPD_REALM, Some(au))
              Redirect(controllers.Wiki.w(wid))
            }) getOrElse
              noPerm(wid, "ADMIN_UOWNER")
          }
        }
    })
  }

  /** move to new parent */
  def wikieMove1(id:String, realm:String=Wikis.RK) =  FAUR("wikie.move") {
    implicit request =>
    for (
      w <- Wikis(realm).findById(id)
    ) yield {
      val parentCats1 = WikiDomain(realm).zEnds(w.wid.cat, "Parent")
      val parents = parentCats1.flatMap {c=>
        Wikis(realm).pages(c).filter(w=>canEdit(w.wid, auth, Some(w)).exists(_ == true)).map(w=>(w.uwid, w.label))
      }
      if(parents.size > 0)
        ROK.k apply { implicit stok =>
          views.html.wiki.wikiMove(w.wid, w, parents)
        }
      else
        Redirect(s"/wiki/id/$id")
    }
  }

  /** move to new parent */
  def wikieMove2(page:String, from:String, to:String, realm:String=Wikis.RK) = FAUR("wikie.move") {
    implicit request =>
    for (
      au <- request.au;
      pageW <- Wikis(realm).findById(page);
      fromW <- Wikis(realm).findById(from);
      toW <- Wikis(realm).findById(to);
      hasP <- pageW.parent.exists(_.toString == from) orErr "does not have a parent"
    ) yield {
      razie.db.tx("Wiki.Move", au.userName) { implicit txn =>
        pageW.update(pageW.copy(parent=Some(toW._id)))
        RMany[WikiLink]("from.id" -> pageW.uwid.id, "to.id" -> fromW.uwid.id, "how"->"Child").toList.foreach{ link=>
          link.delete
          link.copy(to = toW.uwid).create
        }
      }
      Redirect(controllers.Wiki.w(pageW.wid, false)).flashing("count" -> "0")
    }
  }
}
