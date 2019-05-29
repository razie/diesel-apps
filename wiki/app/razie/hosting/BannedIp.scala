/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.hosting

import com.mongodb.casbah.Imports.ObjectId
import org.joda.time.DateTime
import razie.db._

/** register an IP address to be banned
  *
  * todo complete mechanism
  */
@RTable
case class BannedIp (
    ip:String,
    reason:String,
    crDtm: DateTime = DateTime.now,
    _id:ObjectId = new ObjectId()) extends REntity[BannedIp]

/** user factory and utils */
object BannedIps {
  def findId(id:String) = ROne[BannedIp]("_id" -> new ObjectId(id))
  def load = RMany[BannedIp]()

  lazy val all = collection.mutable.Map()++load.toList.map(x=>(x.ip -> "")).toMap

  def isBanned(ip:String) = all.contains(ip)
  def isBanned(ip:Option[String]) = ip.map(all.contains(_)).getOrElse(false)

  def ban(ip:String, reason:String) = {
    BannedIp(ip, reason).create(tx.auto)
    all += ip -> ""
  }
}

