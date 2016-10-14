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
  name: String,
  userId: ObjectId,
  contents: Map[String,String],
  crDtm: DateTime = DateTime.now,
  _id: ObjectId = new ObjectId()) extends REntity[Autosave] {

  override def create(implicit txn: Txn) = RCreate.noAudit[Autosave](this)
  override def update (implicit txn: Txn) = RUpdate.noAudit(Map("_id" -> _id), this)
  override def delete(implicit txn: Txn) = RDelete.noAudit[Autosave](this)
}

/** autosave utils */
object Autosave {

  /** create or update */
  def set(name:String, userId: ObjectId, c:Map[String,String]) =
    ROne[Autosave]("name" -> name, "userId" -> userId).map(_.copy(contents=c).update).getOrElse(Autosave(name, userId, c).create)

  /** each user has its own draft */
  def find(name:String, userId: ObjectId) =
    ROne[Autosave]("name" -> name, "userId" -> userId).map(_.contents)

  /** each user has its own draft */
  def find(name:String, userId : Option[ObjectId]) =
    userId.flatMap(uid=>
      ROne[Autosave]("name" -> name, "userId" -> userId)
    ).map(_.contents)

  /** find or default - will not save the default */
  def OR(name:String, userId: ObjectId, c:Map[String,String]) =
    find(name, userId).getOrElse(c)

  def delete(name:String, userId: ObjectId) = ROne[Autosave]("name" -> name, "userId" -> userId).map(_.delete)
}
