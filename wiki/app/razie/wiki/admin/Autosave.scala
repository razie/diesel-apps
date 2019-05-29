package razie.wiki.admin

import com.mongodb.casbah.Imports._
import org.joda.time.DateTime
import razie.db._
import razie.wiki.model.WID

/** simple support for autosaving drafts - a map / doc store.
  *
  * Make the key something smart, i.e. what.reactor.wpath
  */
@RTable
case class Autosave(
  what: String,                    // kind of object
  realm: String,                   // realm
  name: String,                    // object id in string format, i.e. wpath
  userId: ObjectId,
  contents: Map[String,String],    // actual content autosaved
  ver: Long = 0,                   // auto-increasing version
  crDtm: DateTime = DateTime.now,
  updDtm: DateTime = DateTime.now,
  _id: ObjectId = new ObjectId()) extends REntity[Autosave] {

  override def create(implicit txn: Txn=tx.auto) = RCreate.noAudit[Autosave](this)
  override def update (implicit txn: Txn=tx.auto) = RUpdate.noAudit(Map("_id" -> _id), this)
  override def delete(implicit txn: Txn=tx.auto) = RDelete.noAudit[Autosave](this)
}

/** autosave utils */
object Autosave {

  private def rec(what: String, realm: String, name: String, userId: ObjectId) = {
    ROne[Autosave]("what" -> what, "realm" -> realm, "name" -> name, "userId" -> userId)
  }

  def findAll(what:String, w:WID, userId: ObjectId) =
    RMany[Autosave]("what" -> what, "realm" -> w.getRealm, "name" -> w.wpath, "userId" -> userId)

  /** create or update */
  def set(what: String, wid: WID, userId: ObjectId, c: Map[String, String]) =
    rec(what, wid.getRealm, wid.wpath, userId)
      .map(x => x.copy(contents = c, ver = x.ver + 1, updDtm = DateTime.now).update)
      .getOrElse(Autosave(what, wid.getRealm, wid.wpath, userId, c).create)

  def findForUser(userId: ObjectId) =
    RMany[Autosave]("userId" -> userId)

  /** each user has its own draft */
  def find(what: String, w:WID, userId: ObjectId) =
    rec(what, w.getRealm, w.wpath, userId).map(_.contents)

  /** each user has its own draft */
  def find(wid:WID, userId: Option[ObjectId]) =
    userId.flatMap(uid =>
      rec("wikie", wid.getRealm, wid.wpath, uid)
    ).map(_.contents)

  /** each user has its own draft */
  def find(what: String, w:WID, userId: Option[ObjectId]) =
    userId.flatMap(uid =>
      rec(what, w.getRealm, w.wpath, uid)
    ).map(_.contents)

  /** find or default - will not save the default */
  def OR(what: String, w:WID, userId: ObjectId, c: => Map[String, String]) =
    find(what, w:WID, userId).getOrElse(c)

  // use findAll because sometimes crap is left behind...?
  def delete(iwhat: String, w:WID, userId: ObjectId) : Unit = {
      findAll(iwhat, w, userId).toList.map(_.delete)
  }

  /** filter internal states */
  def activeDrafts(userId: ObjectId) =
    findForUser(userId).filter(x=> x.what != "DomFidPath" && x.what != "DomFidCapture")
}

