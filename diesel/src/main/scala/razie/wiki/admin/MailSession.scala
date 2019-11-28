/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.admin

import razie.hosting.Website
import razie.wiki.model.Wikis

/** wiki specifics */
class MailSession extends BaseMailSession {

  /** name of website, used in subject */
  def website = realm.flatMap(Website.forRealm).getOrElse(Website.dflt)

  /** name of website, used in subject */
  def RK = realm
      .flatMap(Website.forRealm)
      .map(_.label)
      .getOrElse("RacerKidz")

  def SUPPORT = website.supportEmail
  def ADMIN = website.adminEmail

  /** bottom section on each email */
  def bottom =
    Wikis(this.realm.mkString)
      .find("Admin", "template-emails-bottom")
      .orElse(Wikis.rk.find("Admin", "template-emails-bottom"))
      .map(_.content).getOrElse("")

  /** email body section template */
  def text(name: String) =
    Wikis(this.realm.mkString)
      .find("Admin", "template-emails")
      .orElse(Wikis.rk.find("Admin", "template-emails"))
      .flatMap(_.section("template", name))
      .orElse {
        // if section not found in custom, try rk
        Wikis.rk.find("Admin", "template-emails")
       .flatMap(_.section("template", name))
      }
      .map(_.content+bottom).getOrElse("[ERROR] can't find Admin template-emails: " + name)

  /**
    * send an email - just added to the session
    */
  def send(to: String, from: String, subject: String, html: String, bcc: Seq[String] = Seq.empty) {
    val e = new EmailMsg(to, from, subject, html, false, bcc)
    this.emails = e :: this.emails
    e.createNoAudit
  }

  /**
    * send an email - just added to the session. note that there is no special handling of notifications for now
    */
  def notif(to: String, from: String, subject: String, html: String, bcc: Seq[String] = Seq.empty) {
    val e = new EmailMsg(to, from, subject, html, true, bcc)
    this.emails = e :: this.emails
    e.createNoAudit
  }


  def sendSupport(subj:String, name:String, e: String, desc: String, details: String, page:String)(implicit mailSession: MailSession) {
    val html = text("supportrequested").format(name, e, desc, details, page)

    mailSession.notif(SUPPORT, SUPPORT, subj+": " + desc, html)
  }

}

