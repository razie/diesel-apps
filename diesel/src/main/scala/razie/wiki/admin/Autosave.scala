/**
  *  ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
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
  what: String,                     // kind of object
  realm: String,                    // realm
  name: String,                     // object id in string format, i.e. wpath
  userId: ObjectId,
  contents: Map[String,String],     // actual content autosaved
  ver: Long = 0,                    // auto-increasing version
  crDtm:  DateTime = DateTime.now,
  updDtm: DateTime = DateTime.now,
  _id: ObjectId = new ObjectId()) extends REntityNoAudit[Autosave] {

  override def create(implicit txn: Txn=tx.auto) = createNoAudit
  override def update (implicit txn: Txn=tx.auto) = updateNoAudit
  override def delete(implicit txn: Txn=tx.auto) = deleteNoAudit
}

/** autosave utils */
object Autosave {

  def rec(what: String, realm: String, name: String, userId: ObjectId) = {
    ROne[Autosave]("what" -> what, "realm" -> realm, "name" -> name, "userId" -> userId)
  }

  def findAll(what:String, w:WID, userId: ObjectId) =
    RMany[Autosave]("what" -> what, "realm" -> w.getRealm, "name" -> w.wpath, "userId" -> userId)

  /** create or update
    *
    * @param what
    * @param wid
    * @param userId
    * @param c
    * @param editorMsec use it to overwrite updDtm and detect stale data
    */
  def set(what: String, wid: WID, userId: ObjectId, c: Map[String, String], editorMsec:Option[DateTime] = None) =
    rec(what, wid.getRealm, wid.wpath, userId)
      .map(x => x.copy(
        contents = c,
        ver = x.ver + 1,
        updDtm = editorMsec.getOrElse(DateTime.now)
      ).update)
      .getOrElse(Autosave(what, wid.getRealm, wid.wpath, userId, c, 0,
        editorMsec.getOrElse(DateTime.now),
        editorMsec.getOrElse(DateTime.now)
      ).create)

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

  /** each user has its own draft */
  def allDrafts(w:WID) =
    RMany[Autosave]("realm" -> w.getRealm, "name" -> w.wpath)

}


