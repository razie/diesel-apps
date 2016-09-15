package controllers

import org.bson.types.ObjectId
import play.api.mvc.Action
import model.CMDWID
import razie.wiki.Enc
import razie.wiki.model.{WikiEntry, Wikis}

/** basic wiki controller:
  *
  * show a page
  * edit a page (include creating a page
  *
  * optional:
  * browse tags
  * search
  */
class WikiBasic extends RazControllerBase {
  /** show a page */
  def showWid(cw: CMDWID, count: Int, realm: String) = Action { implicit request =>
    // 1. prep the wid
    val iwid = cw.wid.get.r(realm)
    val wid = iwid.formatted

    // 2. find and serve the page
    wid.page.map { we =>
      we.alias.map { wid =>
        // a. redirect a simple alias with no other content
        Redirect(Wikis.w(wid))
      } getOrElse {
        // b. =================== found the page, render it ==================
        we.preprocess(wauth) // just make sure it's processed (parsed)
        val html = Wikis.format(wid, we.markup, wid.content.mkString, Some(we), wauth)
        Ok(html + s"""<hr>| <a href="/wikie/edit/${wid.wpath}">Edit</a> | """).as("text/html")
      }
    } getOrElse
      // c. no page for name, ask to create it
      Ok( s"""Page not found - <a href="/wikie/edit/${wid.wpath}">create it</a>""").as("text/html")
  }

  /** edit an existing or new topic */
  def edit(cw: CMDWID, realm: String) = Action { implicit request =>
    val wid = cw.wid.get.formatted.r(realm)
    val content = wid.content.getOrElse ("[Empty]") // finds the page and loads it if any
    Ok(
      s"""Edit page ${wid.wpath}<p>
         |  <form action="/wikie/save/${wid.wpath}" method="POST">
         |  <textarea id="content" name="content" rows="25" cols="80">$content</textarea><p>
         |  <button type="submit">Save</button>
         |  </form>
       """.stripMargin).as("text/html")
  }

  /** save a new or existing edited topic */
  def save(cw: CMDWID, realm: String) = Action { implicit request =>
    val content = request.body.asFormUrlEncoded.get.apply("content").head
    val wid = cw.wid.get
    razie.db.tx { implicit txn =>
      cw.wid.get.page.map { we =>
        we.update(we.copy(content = content, ver = we.ver + 1)) // udpate existing page
      } getOrElse {
        //cretae new page
        val we = new WikiEntry(wid.cat, wid.formatted.name, wid.name, "md", content, new ObjectId())
        we.create
      }
    }

    Redirect(Wikis.w(wid))
  }

  /** show a tag */
  def showTag(tag: String, realm: String) = Action { implicit request =>
    search(realm, "", "", Enc.fromUrl(tag)).apply(request).value.get.get
  }

  /** search all topics  provide either q or curTags */
  def search(realm: String, q: String, scope: String, curTags: String = "") = Action { implicit request =>
    Ok("Need to implement search")
  }

}
