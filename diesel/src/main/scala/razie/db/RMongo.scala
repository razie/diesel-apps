/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.db

import org.bson.types.ObjectId
import com.mongodb.casbah.Imports._
import com.novus.salat._
import com.novus.salat.annotations._
import org.joda.time.DateTime
import razie.audit.Auditor
import razie.db.RazSalatContext._

/*
 * a simple mongo CRUD entity persistence framework
 *
 * it is dumb on purpose (no relationships etc)
 *
 * all ops are audited by default, using an Auditor service
 */
object RMongo extends SI[Auditor] ("RMongo.Auditor") with razie.Logging {

  def tbl(m: Manifest[_]): String =
    Option(m.runtimeClass.getAnnotation(classOf[RTable])).map(_.value()) match {
      case Some(s) => if (s.length > 0) s else m.runtimeClass.getSimpleName()
      case None => m.runtimeClass.getSimpleName()
    }

  def findById[A <: AnyRef](id: ObjectId)(implicit m: Manifest[A]): Option[A] =
    RazMongo(tbl(m)).findOne(Map("_id" -> id)) map (grater[A].asObject(_))

  def findOne[A <: AnyRef](t: (String, Any)*)(implicit m: Manifest[A]) =
    RazMongo(tbl(m)).findOne(t.toMap) map (grater[A].asObject(_))

  def find[A <: AnyRef](t: (String, Any)*)(implicit m: Manifest[A]) =
    RazMongo(tbl(m)).find(t.toMap).map(grater[A].asObject(_))

  //def grater[Y <: AnyRef](implicit ctx: Context, m: Manifest[Y]): Grater[Y] = ctx.lookup(m.erasure.getName).asInstanceOf[Grater[Y]]

  /** adds the method as[] to an object id */
  implicit class as(id: ObjectId) {
    def as[A <: AnyRef](implicit m: Manifest[A]) = ROne[A](id)
  }

  /***************** auditing **************/
  def aud = getInstance

  /** audit creation of an entity */
  def auditCreate[T](entity: T)(implicit txn: Txn): T =        { audit(aud.logdb(ENTITY_CREATE, s"User: ${txn.user}, txn: ${txn.name}", entity.toString.take(1000))); entity }
  def auditCreatenoaudit[T](entity: T)(implicit txn: Txn): T = { audit(ENTITY_CREATE + " " + entity.toString.take(1000)); entity }
  /** audit update of an entity */
  def auditUpdate[T](entity: T)(implicit txn: Txn): T = { audit(aud.logdb(ENTITY_UPDATE, s"User: ${txn.user}, txn: ${txn.name}", entity.toString.take(1000))); entity }
  def auditUpdatenoaudit[T](entity: T)(implicit txn: Txn): T = { audit(ENTITY_UPDATE + " " + entity.toString.take(1000)); entity }
  /** audit delete of an entity */
  def auditDelete[T](entity: T)(implicit txn: Txn): T = { audit(aud.logdb(ENTITY_DELETE, s"User: ${txn.user}, txn: ${txn.name}", entity.toString.take(1000))); entity }

  final val ENTITY_CREATE = "ENTITY_CREATE"
  final val ENTITY_UPDATE = "ENTITY_UPDATE"
  final val ENTITY_DELETE = "ENTITY_DELETE"
}

/* select/find one mongo item */
object ROne {
  import RMongo._

  def apply[A <: AnyRef](id: ObjectId)(implicit m: Manifest[A]): Option[A] =
    raw(id) map (grater[A].asObject(_))

  def apply[A <: AnyRef](t: (String, Any)*)(implicit m: Manifest[A]) =
    raw(t: _*) map (grater[A].asObject(_))

  def raw[A <: AnyRef](id: ObjectId)(implicit m: Manifest[A]) =
    RazMongo(tbl(m)).findOne(Map("_id" -> id))

  def raw[A <: AnyRef](t: (String, Any)*)(implicit m: Manifest[A]) =
    RazMongo(tbl(m)).findOne(t.toMap)
}

/* select/find many mongo items */
object RMany {
  import RMongo._

  def apply[A <: AnyRef](t: (String, Any)*)(implicit m: Manifest[A]) =
    RazMongo(tbl(m)).find(t.toMap).map(grater[A].asObject(_))

  def raw[A <: AnyRef](t: (String, Any)*)(implicit m: Manifest[A]) =
    RazMongo(tbl(m)).find(t.toMap)

  def sortLimit[A <: AnyRef](query: Map[String, Any], sortby:Map[String,Any], limit:Int = -1)(implicit m: Manifest[A]) =
    (if(limit < 0)
      RazMongo(tbl(m)).find(query)
    else
      RazMongo(tbl(m)).find(query).sort(sortby).limit(limit)
    ).toList.map(grater[A].asObject(_))
}

/* count/find many mongo items */
object RCount {
  import RMongo._

  def apply[A <: AnyRef](t: (String, Any)*)(implicit m: Manifest[A]) =
    RazMongo(tbl(m)).count(t.toMap)
}

/** create a record */
object RCreate {
  import RMongo._

  def apply[A <: AnyRef](t: A)(implicit m: Manifest[A], txn: Txn) =
    RazMongo(tbl(m)) += grater[A].asDBObject(RMongo.auditCreate(t))

  def noAudit[A <: AnyRef](t: A)(implicit m: Manifest[A], txn: Txn) =
    RazMongo(tbl(m)) += grater[A].asDBObject(RMongo.auditCreatenoaudit(t))
}

/** update a record */
object RUpdate {
  import RMongo._

  /** update entity matched by map props */
  def apply[A <: AnyRef](t: Map[String, Any], a: A)(implicit m: Manifest[A], txn: Txn) =
    RazMongo(tbl(m)).update(t, grater[A].asDBObject(RMongo.auditUpdate(a)))

  /** update entity matched by map props */
  def noAudit[A <: AnyRef](t: Map[String, Any], a: A)(implicit m: Manifest[A], txn: Txn) =
    RazMongo(tbl(m)).update(t, grater[A].asDBObject(a))

  /** update entity matched by map props */
  def noAudit[A <: AnyRef](tblName:String, t: Map[String, Any], a: A)(implicit m: Manifest[A], txn: Txn) =
    RazMongo(tblName).update(t, grater[A].asDBObject(a))

  /** update matched on _id */
  def apply[A <: { def _id: ObjectId }](a: A)(implicit m: Manifest[A], txn: Txn) =
    RazMongo(tbl(m)).update(Map("_id" -> a._id), grater[A].asDBObject(RMongo.auditUpdate(a)))

  /** update matched on _id */
  def noAudit[A <: { def _id: ObjectId }](a: A)(implicit m: Manifest[A], txn: Txn) =
    RazMongo(tbl(m)).update(Map("_id" -> a._id), grater[A].asDBObject(a))
}

/** delete a record */
object RDelete {
  import RMongo._

  def apply[A <: AnyRef](t: (String, Any)*)(implicit m: Manifest[A], txn: Txn) =
    RazMongo(tbl(m)).remove(t.toMap)

  def apply[A <: { def _id: ObjectId }](x: A)(implicit m: Manifest[A], txn: Txn) =
    RazMongo(tbl(m)).remove(Map("_id" -> RMongo.auditDelete(x)._id))

  def apply[A <: { def _id: ObjectId }] (tblName:String, x: A)(implicit m: Manifest[A], txn: Txn) =
    RazMongo(tblName).remove(Map("_id" -> RMongo.auditDelete(x)._id))

  def apply (tblName:String, x: Map[String, Any]) (implicit txn: Txn) =
    RazMongo(tblName).remove(RMongo.auditDelete(x))

  def noAudit[A <: { def _id: ObjectId }](x: A)(implicit m: Manifest[A], txn: Txn) =
    RazMongo(tbl(m)).remove(Map("_id" -> x._id))
}

/** base class for entities - provides most common DB ops automatically
  *
  * case class T(a,b) extends REntity[T]
  * new T (a,b).create
  *
  */
class REntity[T <: { def _id: ObjectId }](implicit m: Manifest[T]) { this: T =>
  // had to copy this
  implicit def toroa(id: ObjectId) = new RMongo.as(id)

  def toJsonNice = razie.js.tojsons(razie.js.parse(toJson))
  def toJson = grater[T].asDBObject(this).toString
  def grated = grater[T].asDBObject(this)

  def create(implicit txn: Txn) = RCreate[T](this)
  def update(implicit txn: Txn) = RUpdate[T](this)
  def delete(implicit txn: Txn) = RDelete[T](this)
  def createNoAudit(implicit txn: Txn) = RCreate.noAudit[T](this)
  def updateNoAudit(implicit txn: Txn) = RUpdate.noAudit[T](this)
  def deleteNoAudit(implicit txn: Txn) = RDelete.noAudit[T](this)

  def trash(by:String)(implicit txn: Txn) = {
    WikiTrash(RMongo.tbl(m), grated, by, txn.id).create
    RDelete.noAudit[T](this)
  }
}

/** entities that don't need auditing
  */
class REntityNoAudit[T <: { def _id: ObjectId }](implicit m: Manifest[T]) extends REntity [T] { this: T =>
  override def create(implicit txn: Txn) = createNoAudit
  override def update(implicit txn: Txn) = updateNoAudit
  override def delete(implicit txn: Txn) = deleteNoAudit
}

/** wiki entries are trashed when deleted - a copy of each older version when udpated or deleted
  *
  * todo some ways to recover them
  * */
@RTable
case class WikiTrash(table:String, entry: DBObject, by:String, txnId:String, date:DateTime=DateTime.now, _id: ObjectId = new ObjectId()) {
  def create (implicit txn:Txn) = RCreate.noAudit[WikiTrash](this)
}

