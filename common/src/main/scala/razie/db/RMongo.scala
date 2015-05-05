/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.db

import org.bson.types.ObjectId
import razie.wiki.admin.Auditor
import razie.{cdebug, clog}
import com.mongodb.casbah.Imports._
import com.novus.salat._
import com.novus.salat.annotations._
import razie.db.RazSalatContext._
import razie.wiki.admin._

/*
 * Razie's simple mongo entity persistence framework
 *
 * just playing with this to simplify my code
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
  def auditCreate[T](entity: T): T = { audit(aud.logdb(ENTITY_CREATE, entity.toString)); entity }
  def auditCreatenoaudit[T](entity: T): T = { audit(ENTITY_CREATE + " " + entity.toString); entity }
  /** audit update of an entity */
  def auditUpdate[T](entity: T): T = { audit(aud.logdb(ENTITY_UPDATE, entity.toString)); entity }
  def auditUpdatenoaudit[T](entity: T): T = { audit(ENTITY_UPDATE + " " + entity.toString); entity }
  /** audit delete of an entity */
  def auditDelete[T](entity: T): T = { audit(aud.logdb(ENTITY_DELETE, entity.toString)); entity }

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

  def apply[A <: AnyRef](t: A)(implicit m: Manifest[A], txn: Txn = tx.auto) =
    RazMongo(tbl(m)) += grater[A].asDBObject(RMongo.auditCreate(t))

  def noAudit[A <: AnyRef](t: A)(implicit m: Manifest[A], txn: Txn = tx.auto) =
    RazMongo(tbl(m)) += grater[A].asDBObject(RMongo.auditCreatenoaudit(t))
}

/** update a record */
object RUpdate {
  import RMongo._

  /** update entity matched by map props */
  def apply[A <: AnyRef](t: Map[String, Any], a: A)(implicit m: Manifest[A], txn: Txn = tx.auto) =
    RazMongo(tbl(m)).update(t, grater[A].asDBObject(RMongo.auditUpdate(a)))

  /** update entity matched by map props */
  def noAudit[A <: AnyRef](t: Map[String, Any], a: A)(implicit m: Manifest[A], txn: Txn = tx.auto) =
    RazMongo(tbl(m)).update(t, grater[A].asDBObject(a))

  /** update entity matched by map props */
  def noAudit[A <: AnyRef](tblName:String, t: Map[String, Any], a: A)(implicit m: Manifest[A], txn: Txn = tx.auto) =
    RazMongo(tblName).update(t, grater[A].asDBObject(a))

  /** update matched on _id */
  def apply[A <: { def _id: ObjectId }](a: A)(implicit m: Manifest[A], txn: Txn = tx.auto) =
    RazMongo(tbl(m)).update(Map("_id" -> a._id), grater[A].asDBObject(RMongo.auditUpdate(a)))

  /** update matched on _id */
  def noAudit[A <: { def _id: ObjectId }](a: A)(implicit m: Manifest[A], txn: Txn = tx.auto) =
    RazMongo(tbl(m)).update(Map("_id" -> a._id), grater[A].asDBObject(a))
}

/** delete a record */
object RDelete {
  import RMongo._

  def apply[A <: AnyRef](t: (String, Any)*)(implicit m: Manifest[A], txn: Txn = tx.auto) =
    RazMongo(tbl(m)).remove(t.toMap)

  def apply[A <: { def _id: ObjectId }](x: A)(implicit m: Manifest[A], txn: Txn = tx.auto) =
    RazMongo(tbl(m)).remove(Map("_id" -> RMongo.auditDelete(x)._id))

  def apply[A <: { def _id: ObjectId }] (tblName:String, x: A)(implicit m: Manifest[A], txn: Txn = tx.auto) =
    RazMongo(tblName).remove(Map("_id" -> RMongo.auditDelete(x)._id))

  def apply (tblName:String, x: Map[String, Any]) (implicit txn: Txn = tx.auto) =
    RazMongo(tblName).remove(RMongo.auditDelete(x))

  def noAudit[A <: { def _id: ObjectId }](x: A)(implicit m: Manifest[A], txn: Txn = tx.auto) =
    RazMongo(tbl(m)).remove(Map("_id" -> x._id))
}

/** base class for entities - provides most common DB ops automatically */
class REntity[T <: { def _id: ObjectId }](implicit m: Manifest[T]) { this: T =>
  // had to copy this
  implicit def toroa(id: ObjectId) = new RMongo.as(id)

  def toJson = grater[T].asDBObject(this).toString
  def grated = grater[T].asDBObject(this)

  def create(implicit txn: Txn = tx.auto) = RCreate[T](this)
  def delete(implicit txn: Txn = tx.auto) = RDelete[T](this)
  def update(implicit txn: Txn = tx.auto) = RUpdate[T](this)
  def createNoAudit(implicit txn: Txn = tx.auto) = RCreate.noAudit[T](this)
  def deleteNoAudit(implicit txn: Txn = tx.auto) = RDelete.noAudit[T](this)
  def updateNoAudit(implicit txn: Txn = tx.auto) = RUpdate.noAudit[T](this)
}

