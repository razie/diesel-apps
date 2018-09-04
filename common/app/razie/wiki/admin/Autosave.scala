package razie.wiki.admin

import com.mongodb.casbah.Imports._
import org.joda.time.DateTime
import razie.db._

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

  // todo remove this
  def fix = if("DomFidSpec" == what || "DomFidStory" == what) this.copy(what="wikie") else this

  override def create(implicit txn: Txn=tx.auto) = RCreate.noAudit[Autosave](this.fix)
  override def update (implicit txn: Txn=tx.auto) = RUpdate.noAudit(Map("_id" -> _id), this.fix)
  override def delete(implicit txn: Txn=tx.auto) = RDelete.noAudit[Autosave](this.fix)
}

/** autosave utils */
object Autosave {

  private def rec(iwhat:String, realm:String, name:String, userId: ObjectId) = {
    // todo very ugly trick
    val what = if("DomFidSpec" == iwhat || "DomFidStory" == iwhat) "wikie" else iwhat

    ROne[Autosave]("what" -> what, "realm" -> realm, "name" -> name, "userId" -> userId)
  }

  def findAll(realm:String, name:String, userId: ObjectId) =
    ROne[Autosave]("realm" -> realm, "name" -> name, "userId" -> userId)

  /** create or update */
  def set(what:String, realm:String, name:String, userId: ObjectId, c:Map[String,String]) =
    rec(what, realm, name, userId)
      .map(x=> x.copy(contents=c, ver=x.ver+1, updDtm = DateTime.now).update)
      .getOrElse(Autosave(what, realm, name, userId, c).create)

  def findForUser(userId: ObjectId) =
    RMany[Autosave]("userId" -> userId)

  /** each user has its own draft */
  def find(what:String, realm:String, name:String, userId: ObjectId) =
    rec(what, realm, name, userId).map(_.contents)

  /** each user has its own draft */
  def find(what:String, realm:String, name:String, userId : Option[ObjectId]) =
    userId.flatMap(uid=>
      rec(what, realm, name, uid)
    ).map(_.contents)

  /** find or default - will not save the default */
  def OR(what:String, realm:String, name:String, userId: ObjectId, c: => Map[String,String]) =
    find(what, realm, name, userId).getOrElse(c)

  def delete(what:String, realm:String, name:String, userId: ObjectId) =
    rec(what, realm, name, userId).map(_.delete)

  /** filter internal states */
  def activeDrafts(userId: ObjectId) =
    findForUser(userId).filter(x=> x.what != "DomFidPath" && x.what != "DomFidCapture")
}


