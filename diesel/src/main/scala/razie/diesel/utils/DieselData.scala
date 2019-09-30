/**
  *  ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.utils

import com.mongodb.casbah.Imports._
import org.joda.time.DateTime
import razie.db._

/** simple support for a store - indexed by what+realm+key+user
  */
@RTable
case class DieselData(
  what: String,                    // kind of object
  realm: String,                   // realm
  key: String,           // object id in string format, i.e. wpath
  user: Option[ObjectId],
  contents: Map[String, String],    // actual content autosaved
  ver: Long = 0,                   // auto-increasing version
  crDtm: DateTime = DateTime.now,
  updDtm: DateTime = DateTime.now,
  _id: ObjectId = new ObjectId()) extends REntity[DieselData] {

  override def create(implicit txn: Txn = tx.auto) = RCreate.noAudit[DieselData](this)

  override def update(implicit txn: Txn = tx.auto) = RUpdate.noAudit(Map("_id" -> _id), this)

  override def delete(implicit txn: Txn = tx.auto) = RDelete.noAudit[DieselData](this)
}

object DieselData {

  private def rec(what: String, realm: String, key: String, userId: Option[ObjectId]=None) = {
    ROne[DieselData]("what" -> what, "realm" -> realm, "key" -> key, "userId" -> userId)
  }

  /** create or update */
  def set(what: String, realm: String, key: String, userId: Option[ObjectId], c: Map[String, String]) =
    rec(what, realm, key, userId)
        .map(x => x.copy(contents = c, ver = x.ver + 1, updDtm = DateTime.now).update)
        .getOrElse(DieselData(what, realm, key, userId, c).create)

  /** create or update */
  def update(what: String, realm: String, key: String, userId: Option[ObjectId], c: Map[String, String]) =
    rec(what, realm, key, userId)
        .map(x => x.copy(contents = x.contents ++ c, ver = x.ver + 1, updDtm = DateTime.now).update)
        .getOrElse(DieselData(what, realm, key, userId, c).create)

  /** each user has its own draft */
  def find(what: String, realm: String, key: String, userId: Option[ObjectId]=None) = {
    rec(what, realm, key, userId)
  }

  // use findAll because sometimes crap is left behind...?
  def delete(what: String, realm: String, key: String, userId: Option[ObjectId]=None): Unit = {
    rec(what, realm, key, userId).toList.map(_.delete)
  }
}




