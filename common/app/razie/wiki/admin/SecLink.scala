/**
 * ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.admin

import org.bson.types.ObjectId
import org.joda.time.DateTime
import razie.db._
import razie.wiki.Services
import razie.db.tx.txn

/**
 * secured link with expiry date, to be emailed for instance, like "activate your account"
 *
 * the idea is that the info required (account number, email whatever) is saved on the server side and given a unique ID/hash which is emailed.
 *
 * @see razie.controllers.Sec
 *
 *      1. create the DoSec
 *      2. get the securl - this will create it as well
 *      3. when used, it will redirect
 *      4. purge those that are done or expired whenever
 *
 *      Note that this is persisted only if the secUrl is requested
 */
@RTable
case class SecLink(
                    link: String,
                    host: Option[String] = None,
                    maxCount: Int = 1, // TODO use this somehow - not certain they're done though...
                    expiry: DateTime = DateTime.now.plusHours(8),
                    count: Int = 0, // how many times it was done
                    lastDoneDtm: DateTime = DateTime.now,
                    _id: ObjectId = new ObjectId()) {
  //extends REntity[DoSec] {

  def id = _id.toString

  def isDone = count >= maxCount

  private def create = RCreate(this)

  /** update status as done - you should purge these entries every now and then */
  def done = RUpdate(this.copy(count = this.count + 1, lastDoneDtm = DateTime.now))

  /** invoke this only once - create the link, persist it and make a unique URL
    *
    * this must be a def - otherwise it keeps creating it, eh?
    */
  def secUrl = {
    create
    if (host.isDefined)
      "http://" + host.get + SecLink.PREFIX + id
    else
      "http://" + Services.config.hostport + SecLink.PREFIX + id
  }
}

/** utilities to find/purge sec links */
object SecLink {
  val PREFIX = "/doe/sec/"

  /** find by id - this is used from controller when link is invoked */
  def find(id: String) = {
    // TODO play 20 workaround - remove this in play 2.1
    val iid = id.replaceAll(" ", "+")

    // bad id causes exception
    try {
      ROne[SecLink](new ObjectId(iid))
    } catch {
      case t:Throwable => None
    }
  }
}
