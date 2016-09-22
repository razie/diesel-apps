/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.admin

import java.util._
import java.util.concurrent.TimeUnit
import javax.mail._
import javax.mail.internet._

import akka.actor.{Actor, Props}
import org.bson.types.ObjectId
import org.joda.time.DateTime
import play.libs.Akka
import razie.base.Audit
import razie.db.{RMany, ROne, REntity, RTable}
import razie.wiki.Services
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import razie.clog

/** a prepared email to send - either send now, later, backup etc */
@RTable
case class EmailMsg(
  to: String,
  from: String,
  subject: String,
  html: String,
  isNotification: Boolean = true,
  bcc: Seq[String] = Seq.empty,
  status: String = EmailMsg.STATUS.READY,
  lastError: String = "",
  sendCount: Integer = 0,
  senderNode: Option[String] = None,
  lastDtm: DateTime = DateTime.now(),
  _id: ObjectId = new ObjectId()) extends REntity[EmailMsg] {
  def shouldResend = (EmailMsg.RESEND contains status) && (sendCount < EmailMsg.MAX_RETRY_COUNT)
}

/** email statics */
object EmailMsg {
  /** statuses for email messages */
  object STATUS {
    final val READY = "ready"
    final val SENDING = "sending" // while picked up in memory for re-delivery
    final val OOPS = "oops"
    final val FAILED = "failed"
    final val SKIPPED = "skipped"
  }

  // resending in these states
  val RESEND = Array (STATUS.READY, STATUS.OOPS)

  // will stop trying to send after X attempts
  val MAX_RETRY_COUNT = 100
}

/**
 * a mail session that doesn't have to connect to the server if there's nothing to send...
 *
 * you get one with SendEmail.withSession and when closed, it will spawn a thread to send the emails collected during...
 */
class MailSession(implicit mailSession: Option[Session] = None) {
  var emails: scala.List[EmailMsg] = Nil // collecting messages here to be sent at end

  lazy val session = mailSession getOrElse SendEmail.mkSession(false)
  private var transport:Option[Transport] = None;

  var count = 0;

  def send (msg:MimeMessage) = {
    // todo I assume it has at least one recipient
    try {
      if(transport.isEmpty)
        transport=Some(session.getTransport(msg.getAllRecipients()(0)))
      if(!transport.get.isConnected) {
        transport.get.connect()
        Audit.logdb("EMAIL_STATUS", "MAIL.CONNECT status="+transport.map(_.isConnected).mkString)
      }

      count +=1
      transport.get.sendMessage(msg, msg.getAllRecipients);
    } finally {
    }
  }

  def close = {
    if(transport.isDefined && transport.get.isConnected) transport.get.close()
    Audit.logdb("EMAIL_STATUS", "MAIL.CLOSE status="+transport.map(_.isConnected).mkString + " sent "+count +" emails")
  }
}

/** the email sender - saves emails in DB and sends them asynchronously, retrying in certain cases */
object SendEmail extends razie.Logging {
  import EmailMsg.STATUS

  /** this is set to false for normal testing - set to true for quick testing and stress/perf testing */
  val NO_EMAILS = true
  /** programatically set to false during a test */
  var NO_EMAILS_TESTNG = true

  // should be lazy because of akka's bootstrap
  lazy val emailSender = Akka.system.actorOf(Props[EmailSender], name = "EmailSender")

  def init = {emailSender.path} // initialize lazy

  // set from Global, with your actual user/pass/email server combo
  var mkSession : (Boolean) => javax.mail.Session = {debug:Boolean=>
    val props = new Properties();
    props.put("mail.smtp.auth", "true");
    props.put("mail.smtp.starttls.enable", "true");
    props.put("mail.smtp.host", "smtp.gmail.com");
    props.put("mail.smtp.port", "587");

    val session = javax.mail.Session.getInstance(props,
      new SMTPAuthenticator("your@email.com", "big secret"))

    session.setDebug(debug);
    session
  }

  /**
   * send an email - just added to the session
   */
  def send(to: String, from: String, subject: String, html: String, bcc: Seq[String] = Seq.empty)(implicit mailSession: MailSession) {
    val e = new EmailMsg(to, from, subject, html, false, bcc)
    mailSession.emails = e :: mailSession.emails
    e.createNoAudit
  }

  /**
   * send an email - just added to the session. note that there is no special handling of notifications for now
   */
  def notif(to: String, from: String, subject: String, html: String, bcc: Seq[String] = Seq.empty)(implicit mailSession: MailSession) {
    val e = new EmailMsg(to, from, subject, html, true, bcc)
    mailSession.emails = e :: mailSession.emails
    e.createNoAudit
  }

  /**
   * collects all emails in session and spawns thread at end of session to send them asynchornously.
   *
   * sent emails are audited as well as failures
   */
  def withSession[C](body: (MailSession) => C): C = {
    implicit val mailSession = new MailSession
    val res = body(mailSession)

    emailSender ! mailSession // spawn sender

    res
  }

  val STATE_OK      = "ok" // all ok
  val STATE_MAXED   = "maxed" // last sending resulted in too many emails - should wait a bit
  val STATE_BACKOFF = "backoff" // last sending resulted in some error - backoff for a bit

  val CMD_TICK = "CMD_TICK"        // it tries to resed on a TICK every now and then
  val CMD_RESEND = "CMD_RESEND"    // admin command sent somehow
  val CMD_RESTARTED = "CMD_RESTARTED"        // when process restarted - see what was in progress

  var curCount = 0;            // current sending queue size
  var state = STATE_OK

  def setState (news:String) = {
    if(state != news) Audit.logdb("EMAIL_STATE", s"from $state to $news")
    state = news
  }

  // todo improve to include port
  def getNodeId = {
    Services.config.node
  }

  /** email sender - also acts on a timer for retries
    *
    * messages: email recovered, new email, new email session, tick, resend
    */
  private class EmailSender extends Actor {
    def receive = {
//      case id: ObjectId => ROne[EmailMsg](id).fold {curCount -= 1} (e => isend(e, new MailSession))

      case e: EmailMsg => {
        Audit.logdb("ERR_EMAIL_PROC", "SOMEBODY SENT AN EMAIL DIRECTLY... find and kill "+e.toString)
//        isend(e, new MailSession)
      }

      case mailSession: MailSession => {
        //todo decrement if maxed below, also if ok keep sending only until the first error
        Audit.logdb("EMAIL_STATUS", s"MAIL.process session ${mailSession.emails.size} emails")
        try {
          if (state == STATE_OK) {
            curCount += mailSession.emails.size
            mailSession.emails.reverse.collect {
              // reuse session but on first failure, stop sending
              case e: EmailMsg if state == STATE_OK => isend(e, mailSession)
              case e: EmailMsg if state != STATE_OK => curCount -= 1
            }
          } else {
            Audit.logdb("ERR_EMAIL_MAXED", s"EmailSender received ${mailSession.emails.size} messages while not OK, scheduling just one")
            mailSession.emails.lastOption.map(_._id).flatMap(id=>ROne[EmailMsg](id)).fold {} (e => isend(e, mailSession))
          }
        } finally {
          mailSession.close
          maybeBackoff
        }
      }

      // check for messages to retry
      case sss @ CMD_TICK => {
//        Audit.logdb("EMAIL_STATUS", s"CMD_TICK EmailSender $sss state=$state")
        resend
      }

      // resend all messages that were sent too many times
      case sss @ CMD_RESEND => {
        val x = RMany[EmailMsg]().filter(_.sendCount >= EmailMsg.MAX_RETRY_COUNT).toList
        Audit.logdb("EMAIL_STATUS", s"CMD_RESEND EmailSender $sss state=$state resending "+x.size)
        x.foreach { g =>
          // update all failed for resending once...
          g.copy(sendCount = EmailMsg.MAX_RETRY_COUNT - 1).updateNoAudit
        }
        resend
      }

      // INIT - upon restart check for messages to retry that were being sent from this node
      case ssss @ (CMD_RESTARTED) => {
        clog << s"EmailSender $ssss state=$state"
          RMany[EmailMsg]().filter(x=> x.status == STATUS.SENDING && (x.senderNode.isEmpty || x.senderNode.exists(_ == getNodeId))).grouped(50).foreach(g => {
            val gg = g.toList.map { ie =>
              // reset the messages to oops from sending
              val e = ie.copy(status = STATUS.OOPS, senderNode = Some(getNodeId))
              e.updateNoAudit
              e
            }
            val s = new MailSession()
            s.emails = gg
            emailSender ! s
            clog << s"EmailSender resent: ${s.emails.size}"
          })
        }
    }

    def maybeBackoff: Unit = {
      if (state == STATE_BACKOFF) {
        Audit.logdb("EMAIL_STATUS", "MAIL.backing off")
        Akka.system.scheduler.scheduleOnce(
          Duration.create(1, TimeUnit.MINUTES),
          this.self,
          CMD_TICK)
      }
    }

    def resend : Unit = {
      if(state != STATE_OK) {
        // send just one ping - if successful, it will resend all
        RMany[EmailMsg]().find(_.shouldResend).foreach(g => {
          val s = new MailSession() {
            override def close = {
              if (state == STATE_OK) resend
              super.close
            }
          }
          s.emails = g :: Nil
          emailSender ! s
          Audit.logdb("EMAIL_STATUS", s"MAIL.resending.PING EmailSender resent: ${s.emails.size}")
        })
      } else {
        // resend all
        RMany[EmailMsg]().filter(_.shouldResend).grouped(50).foreach(g => {
          val s = new MailSession()
          s.emails = g.toList
          emailSender ! s
          Audit.logdb("EMAIL_STATUS", s"MAIL.resending EmailSender resent: ${s.emails.size}")
        })
      }
    }

    // upon start, reload ALL messages to send - whatever was not sent last time
    override def preStart(): Unit = {
      Akka.system.scheduler.schedule(
        Duration.create(30, TimeUnit.SECONDS),
        Duration.create(30, TimeUnit.MINUTES),
        this.self,
        CMD_TICK)
      Akka.system.scheduler.scheduleOnce(
        Duration.create(10, TimeUnit.SECONDS),
        this.self,
        CMD_RESTARTED)
    }

    /** send an email */
    private def isend(ie: EmailMsg, mailSession: MailSession) = {
      // check: anyone got to it before I did?
      ROne[EmailMsg](ie._id).filter(x=> x.shouldResend && x.sendCount == ie.sendCount).map {orig=>
        val e = ie.copy(status = STATUS.SENDING, sendCount = ie.sendCount + 1, lastDtm = DateTime.now(), senderNode = Some(getNodeId))
        e.updateNoAudit

        if (Services.config.hostport.startsWith("test") && NO_EMAILS_TESTNG ||
          Services.config.isLocalhost && (e.isNotification || NO_EMAILS || NO_EMAILS_TESTNG)) {
          // don't send emails when running test mode
          Audit.logdb("EMAIL_SENT_NOT", Seq("to:" + e.to, "from:" + e.from, "subject:" + e.subject, "body:" + e.html).mkString("\n"))
          e.copy(status = STATUS.SKIPPED, lastDtm = DateTime.now()).updateNoAudit
        } else
          try {
            val message: MimeMessage = mkMsg(e, mailSession)

            mailSession.send(message);

            Audit.logdb("EMAIL_SENT", Seq("to:" + e.to, "from:" + e.from, "subject:" + e.subject).mkString("\n"))
            e.deleteNoAudit

            // this is dealt with in resend - ping
//            if (state != STATE_OK)
//              resend // seems ok now: reload all messages that were waiting

            setState(STATE_OK) // i can send messages for sure now, eh?
          } catch {
            case mex: MessagingException => {
              e.copy(status = STATUS.OOPS, lastDtm = DateTime.now(), lastError = mex.toString).updateNoAudit
              Audit.logdb("ERR_EMAIL_SEND",
                Seq("to:" + e.to, "from:" + e.from,
                  "subject:" + e.subject, "html=" + e.html.take(100),
                  "EXCEPTION = " + mex.toString()).mkString("\n"))
              if (mex.toString contains "quota exceeded")
                setState(STATE_MAXED)
              else
                setState(STATE_BACKOFF)
              error("ERR_EMAIL_SEND", mex)
            }
          } finally {
            curCount -= 1
          }
      }

    }
    private def mkMsg(e: EmailMsg, mailSession: MailSession): MimeMessage = {
      val message = new MimeMessage(mailSession.session);
      message.setFrom(new InternetAddress(e.from));
      message.addRecipient(Message.RecipientType.TO, new InternetAddress(e.to));
      e.bcc.foreach { b =>
        message.addRecipient(Message.RecipientType.BCC, new InternetAddress(b));
      }

      message.setSubject(e.subject)

      val multipart = new MimeMultipart();
      val htmlPart = new MimeBodyPart();
      htmlPart.setContent(e.html, "text/html");
      htmlPart.setDisposition("inline"); //BodyPart.INLINE);

      // PREPARE THE IMAGE
      //      val imgPart = new MimeBodyPart();
      //
      //      val fileName = "public/racerkidz-small.png";
      //
      //      var classLoader = Thread.currentThread().getContextClassLoader();
      //      if (classLoader == null) {
      //        classLoader = this.getClass().getClassLoader();
      //        if (classLoader == null) {
      //          throw new IllegalStateException("IT IS NULL AGAIN!!!!");
      //        }
      //      }
      //
      //      val ds = new URLDataSource(classLoader.getResource(fileName));
      //
      //      imgPart.setDataHandler(new DataHandler(ds));
      //      imgPart.setHeader("Content-ID", "<logoimg_cid>");
      //      imgPart.setDisposition("inline"); //MimeBodyPart.INLINE);
      //      imgPart.setFileName("logomailtemplate.png");

      multipart.addBodyPart(htmlPart);
      //      multipart.addBodyPart(imgPart);
      message.setContent(multipart);

      //            Transport.send(message);
      message
    }
  }
}

/** simple SMTP user/pass authenticator */
class SMTPAuthenticator (userName:String, password:String) extends javax.mail.Authenticator {
  override def getPasswordAuthentication() = {
    new PasswordAuthentication(userName, password);
  }
}

