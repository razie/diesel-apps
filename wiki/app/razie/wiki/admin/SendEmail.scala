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
import razie.audit.Audit
import razie.db._
import razie.wiki.Services

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import razie.clog
import razie.hosting.Website

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

  override def createNoAudit(implicit txn: Txn=tx.auto): Unit = super.createNoAudit(txn)
  override def updateNoAudit(implicit txn: Txn=tx.auto): Unit = super.updateNoAudit(txn)
  override def deleteNoAudit(implicit txn: Txn=tx.auto): Unit = super.deleteNoAudit(txn)
}

/** email statics */
object EmailMsg {
  /** statuses for email messages */
  object STATUS {
    final val READY = "ready"
    final val SENDING = "sending" // while picked up in memory for re-delivery
    final val OOPS = "oops"
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
  *
  * it is a lot more optimzed sending emails in a batch
 */
class BaseMailSession(implicit mailSession: Option[Session] = None) {
  var emails: scala.List[EmailMsg] = Nil // collecting messages here to be sent at end
  var realm : Option[String] = None

  // if user defined, we need special props - defaults to gmail
  def doesItNeedGmail = realm.flatMap(Website.forRealm).getOrElse(Website.dflt).prop("mail.smtp.user").isDefined

  var needsGmail = doesItNeedGmail // likely with the wrong realm at this time...

  //in case you serve multiple websites - which one is in this session?
  // there are subject and reply email that may differ
  def withRealm (s:Option[String]) = {
    realm=s;
    needsGmail = doesItNeedGmail
    this
  }

  // use local session
  lazy val session = mailSession getOrElse SendEmail.mkSession(
    this,
    !needsGmail, //true //emails.headOption.exists(_.isNotification), // let's do notifications for now
    false
  )
  private var transport:Option[Transport] = None;

  var count = 0;

  /** the actual email send
    *
    * exceptions are caught outside of this - see call site */
  def send (msg:MimeMessage) = {
    try {
      if(transport.isEmpty)
        // todo I assume it has at least one recipient
        transport=Some(session.getTransport(msg.getAllRecipients()(0)))
      if(!transport.get.isConnected) {
        clog << "EMAIL_CONNECT"
        transport.get.connect()
        Audit.logdb("EMAIL_STATUS", "MAIL.CONNECT status="+transport.map(_.isConnected).mkString)
      }

      count +=1
      transport.get.sendMessage(msg, msg.getAllRecipients);
    } finally {
    }
  }

  def close = {
    if(transport.isDefined && transport.get.isConnected) {
      clog << "EMAIL_CLOSE"
      transport.get.close()
    }
    Audit.logdb("EMAIL_STATUS", "MAIL.CLOSE status="+transport.map(_.isConnected).mkString + " sent "+count +" emails")
  }
}

/** the email sender - saves emails in DB and sends them asynchronously with an actor, retrying in certain cases
  *
  * turns out that sending email is not the easiest thing to do
  *
  * will recover on restart, can be restarted etc
  *
  * todo move into a connectors subsystem
  */
object SendEmail extends razie.Logging {
  import EmailMsg.STATUS

  /** prevent it from actually sending out, good for testing
    * this is set to false for normal testing - set to true for quick testing and stress/perf testing */
  val NO_EMAILS = true

  /** prevent for sending out * programatically set to false during a test */
  var NO_EMAILS_TESTNG = true

  // should be lazy because of akka's bootstrap
  lazy val emailSender = Akka.system.actorOf(Props[EmailSender], name = "EmailSender")

  def init = {
    emailSender.path
  } // initialize lazy

  // set this from Global/Main, with your actual user/pass/email server combo
  var mkSession : (BaseMailSession, Boolean, Boolean) => javax.mail.Session = {(msession:BaseMailSession, test:Boolean,debug:Boolean)=>
    val props = new Properties();

    val session = if(test) {

      props.put("mail.smtp.host", "localhost");
      javax.mail.Session.getInstance(props,
        new SMTPAuthenticator("your@email.com", "big secret"))

    } else {

      props.put("mail.smtp.auth", "true");
      props.put("mail.smtp.starttls.enable", "true");
      props.put("mail.smtp.host", "smtp.gmail.com");
      props.put("mail.smtp.port", "587");
      javax.mail.Session.getInstance(props,
        new SMTPAuthenticator("your@email.com", "big secret"))
    }

    session.setDebug(debug || test);
    session
  }

  /**
   * collects all emails in session and spawns thread at end of session to send them asynchornously.
   *
   * sent emails are audited as well as failures
   */
  def withSession[C](realm:String="rk")(body: (MailSession) => C): C = {
    implicit val mailSession = (new MailSession).withRealm(Some(realm)).asInstanceOf[MailSession]
    val res = body(mailSession) // supposed to collect emails in the session

    emailSender ! mailSession // spawn sender

    res
  }

  val STATE_OK      = "ok" // all ok
  val STATE_MAXED   = "maxed" // last sending resulted in too many emails - should wait a bit
  val STATE_BACKOFF = "backoff" // last sending resulted in some error - backoff for a bit
  val STATE_BACKEDOFF = "backedoff" // last sending resulted in some error - backoff for a bit

  val CMD_TICK = "MAIL_CMD_TICK"        // it tries to resed on a TICK every now and then
  val CMD_RESEND = "MAIL_CMD_RESEND"    // admin command sent somehow
  val CMD_SEND10 = "MAIL_CMD_SEND10"    // admin
  val CMD_STOPEMAILS = "MAIL_CMD_STOPEMAILS"    //
  val CMD_RESTARTED = "MAIL_CMD_RESTARTED"        // when process restarted - see what was in progress

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
    var lastBackoff = System.currentTimeMillis() - 600000

    def receive = {
      case e: EmailMsg => {
        Audit.logdb("ERR_EMAIL_PROC", "SOMEBODY SENT AN EMAIL DIRECTLY... find and kill " + e.toString)
      }

      case mailSession: MailSession => {
        //todo decrement if maxed below, also if ok keep sending only until the first error
        Audit.logdb("EMAIL_STATUS", s"$state MAIL.process session ${mailSession.emails.size} emails")

        // 1. does it need gmail for hotmail? - these receivers need emails sent from a gmail address
        def needs(s: String) =
          s.contains("hotmail.com") ||
            s.contains("live.com") ||
            s.contains("outlook.com") ||
            s.contains("live.ca") ||
            s.contains("hotmail.ca")

        val mcount = mailSession.emails.count(e => needs(e.to))

        if (mcount != 0 && mcount != mailSession.emails.size) {
          // split in two sessions
          val m1 = new MailSession
          val m2 = new MailSession

          mailSession.emails.collect {
            case e if needs(e.to) => m1.emails = e :: m1.emails
            case e => m2.emails = e :: m2.emails
          }

          m1.needsGmail = true

          emailSender ! m1 // spawn sender
          emailSender ! m2 // spawn sender

        } else {

          mailSession.needsGmail = mailSession.doesItNeedGmail || mcount == mailSession.emails.size

          // 2. see about sending it

          try {
            if (state == STATE_OK) {
              curCount += mailSession.emails.size

              mailSession.emails.reverse.collect {
                // reuse session but on first failure, stop sending
                case e: EmailMsg if state == STATE_OK => isend(e, mailSession)
                case e: EmailMsg if state != STATE_OK => curCount -= 1
              }
            } else {

              if (System.currentTimeMillis() - lastBackoff > 15000) {
                // don't send another test email sooner than half min
                Audit.logdb("EMAIL_STATUS", s"$state EmailSender received ${mailSession.emails.size} messages while not OK, scheduling just one")
                mailSession.emails.lastOption.map(_._id).flatMap(id => ROne[EmailMsg](id)).fold {}(e => isend(e, mailSession))
              } else {
                Audit.logdb("EMAIL_STATUS", s"$state EmailSender received ${mailSession.emails.size} messages while not OK, NONE attempted")
              }
            }
          } finally {
            mailSession.close
            maybeBackoff
          }
        }
      }

      // check for messages to retry
      case sss @ CMD_TICK => {
        resend()
      }

      // admin command - resend all messages that were sent too many times
      case sss @ CMD_SEND10 => {
        val cnt = 10 - RMany[EmailMsg]().filter(_.shouldResend).size

        val x = RMany[EmailMsg]().filter(_.sendCount >= EmailMsg.MAX_RETRY_COUNT).take(cnt).toList

        Audit.logdb("EMAIL_STATUS", s"$state CMD_RESEND EmailSender $sss state=$state updating "+x.size)
        x.foreach { g =>
          // update all failed for resending once...
          g.copy(sendCount = EmailMsg.MAX_RETRY_COUNT - 1).updateNoAudit
        }
        resend()
      }

      // admin command - resend all messages that were sent too many times
      case sss @ CMD_RESEND => {
        val x = RMany[EmailMsg]().filter(_.sendCount >= EmailMsg.MAX_RETRY_COUNT).toList
        Audit.logdb("EMAIL_STATUS", s"CMD_RESEND EmailSender $sss state=$state resending "+x.size)
        x.foreach { g =>
          // update all failed for resending once...
          g.copy(sendCount = EmailMsg.MAX_RETRY_COUNT - 1).updateNoAudit
        }
        resend()
      }

      // admin command - stop sending all existing emails
      case sss @ CMD_STOPEMAILS => {
        val x = RMany[EmailMsg]().filter(_.sendCount < EmailMsg.MAX_RETRY_COUNT).toList
        Audit.logdb("EMAIL_STATUS", s"CMD_STOPEMAILS EmailSender $sss state=$state suspending: "+x.size)
        x.foreach { g =>
          // update all failed for resending once...
          g.copy(sendCount = EmailMsg.MAX_RETRY_COUNT).updateNoAudit
        }
        resend()
      }

      // INIT - upon restart check for messages to retry that were being sent from this node
      case ssss @ (CMD_RESTARTED) => {
        Audit.logdb("EMAIL_STATUS", s"$ssss state=$state")
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
            Audit.logdb("EMAIL_STATUS", s"EmailSender resent: ${s.emails.size}")
          })
        }
    }

    def maybeBackoff: Unit = {
      if (state == STATE_BACKOFF) {
        Audit.logdb("EMAIL_STATUS", s"$state MAIL.backing off")
        Akka.system.scheduler.scheduleOnce(
          Duration.create(1, TimeUnit.MINUTES),
          this.self,
          CMD_TICK)
        lastBackoff = System.currentTimeMillis()
        setState(STATE_BACKEDOFF)
      }
    }

    def resend (howmany:Int = -1) : Unit = {
      clog << "EMAIL_RESEND"
      if(state == STATE_OK) {
        // resend all
        (if(howmany > 0)
          scala.List(RMany[EmailMsg]().filter(_.shouldResend).take(howmany).toSeq)
        else
          RMany[EmailMsg]().filter(_.shouldResend).grouped(50).toList
        ).foreach(g => {
          val s = new MailSession()
          s.emails = g.toList
          emailSender ! s
          Audit.logdb("EMAIL_STATUS", s"$state MAIL.resending EmailSender resent: ${s.emails.size}")
        })
      } else if(System.currentTimeMillis() - lastBackoff > 50000) {
        // send just one ping - if successful, it will resend all
        RMany[EmailMsg]().find(_.shouldResend).foreach(g => {
          val s = new MailSession() {
            override def close = {
              if (state == STATE_OK) resend() // state became ok... resend more
              super.close
            }
          }
          s.emails = g :: Nil
          emailSender ! s
          Audit.logdb("EMAIL_STATUS", s"$state MAIL.resending.PING EmailSender resent: ${s.emails.size}")
        })
      }
    }

    // upon start, reload ALL messages to send - whatever was not sent last time
    override def preStart(): Unit = {
      clog << "RESTARTING EMAIL sender"
      context.system.scheduler.schedule(
        Duration.create(30, TimeUnit.SECONDS),
        Duration.create(30, TimeUnit.MINUTES),
        this.self,
        CMD_TICK)
      context.system.scheduler.scheduleOnce(
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

        // for testing, we're not sending emails
        if (Services.config.hostport.startsWith("test") && NO_EMAILS_TESTNG ||

          Services.config.isLocalhost && (e.isNotification || NO_EMAILS || NO_EMAILS_TESTNG)) {
          // don't send emails when running test mode
          Audit.logdb("EMAIL_SENT_NOT", Seq("to:" + e.to, "from:" + e.from, "subject:" + e.subject, "body:" + e.html).mkString("\n"))
          e.copy(status = STATUS.SKIPPED, lastDtm = DateTime.now()).updateNoAudit

        } else

          try {
            val message: MimeMessage = mkMsg(e, mailSession)

            clog << "EMAIL_SENDING: " + Seq("to:" + e.to, "from:" + e.from, "subject:" + e.subject).mkString("\n")

            // todo one way to speed it up is to send one email to many people instead of individualized emails
            val t1 = System.currentTimeMillis()
            mailSession.send(message);
            var t2 = System.currentTimeMillis()
            val dur = t2-t1

            Audit.logdb("EMAIL_SENT", state + " "+dur+"msec"+Seq(" to:" + e.to, "from:" + e.from, "subject:" + e.subject, "body:" + e.html).mkString("\n"))
            e.deleteNoAudit

            if(dur > 30000) {
              Audit.logdb("EMAIL_STATUS", "sending slowing down, closing connection")
              setState(STATE_BACKEDOFF)
              Akka.system.scheduler.scheduleOnce(
                Duration.create(50, TimeUnit.SECONDS),
                this.self,
                CMD_TICK)
            } else {
              setState(STATE_OK) // i can send messages for sure now, eh?
            }
          } catch {
            case mex: Throwable => {
              e.copy(status = STATUS.OOPS, lastDtm = DateTime.now(), lastError = mex.toString).updateNoAudit
              Audit.logdb("ERR_EMAIL_SEND",
                Seq("to:" + e.to, "from:" + e.from,
                  "subject:" + e.subject, "html=" + e.html.take(100),
                  "EXCEPTION = " + mex.toString()).mkString("\n"))
              if (mex.toString contains "quota exceeded") {
                Audit.logdb("ERR_EMAIL_MAXED", mex.toString)
                setState(STATE_MAXED)
              } else {
                setState(STATE_BACKOFF)
                Audit.logdb("ERR_EMAIL_BACKING OFF", mex.toString)
              }
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

