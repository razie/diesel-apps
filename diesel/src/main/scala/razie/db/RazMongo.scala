/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.db

import com.mongodb.DBObject
import com.mongodb.casbah.{MongoDB, MongoCollection}
import com.novus.salat.{Context, NeverTypeHint}
import org.joda.time.DateTime
import scala.util.Try
import com.mongodb.casbah.Imports._

/** don't remember why i need this... but i somehow do... */
package object RazSalatContext {
  implicit val ctx = new Context {
    val name = "When-Necessary-Context"
    override val typeHintStrategy = NeverTypeHint
  }
}

/** represents a mongo db instance
  *
  * we limit our usage to primitives that can be implemented by any rather stupid JSON document store.
  *
  */
object RazMongo extends SI[MongoDB] ("MongoDB") {

  def db = getInstance

  // upgrades handled in Global

  /** important during the upgrades themselves - you can't recursively use db */
  def withDb[B](d: MongoCollection, reason:String="-")(f: MongoCollection => B) =
    dbop(s"withDb ${d.name} reason=$reason") {
      f(d);
    }

  def collectionNames = db.collectionNames filter (!_.startsWith("system."))

  /** this is the main factory for db tables
    *
    * todo you need a way to overwrite this to use another persistance
    */
  def apply(table: String) = new RazMongoTable(table)

  def findUpgrade (name:String) = apply("Ver").findOne(Map("name" -> name))
  def didUpgrade (name:String) = {
    apply("Ver") += Map("name" -> name, "dtm" -> DateTime.now)
  }
  def upgradeMaybe (name:String, prereq : Array[String])(body : => Unit) = {
    if(! findUpgrade(name).isDefined) {
      body
      didUpgrade(name)
    }
  }

  /** wrap all access to the DB object  - potentially use rk as a store */
  trait RazTable {
    type CursorType <: scala.Iterator[com.mongodb.casbah.Imports.DBObject]
    def name:String

    def exists :Boolean
    def drop:Unit

    def size :Long
    def count(pairs: Map[String, Any]):Long

    def +=(o: DBObject):Unit
    def +=(pairs: Map[String, Any]):Unit

    def findAll():CursorType
    def find(pairs: Map[String, Any]):CursorType
    def findOne(pairs: Map[String, Any]) : Option[DBObject]
    def remove(pairs: Map[String, Any]):Unit
    def update(pairs: Map[String, Any], o: DBObject):Unit
    def save(o:DBObject):Unit
  }

  /** wrap all access to the DB object
    *
    * if you want to use something else for storage,
    * overwrite this class and the factory RazMongo.apply
    */
  class RazMongoTable(val name: String) extends RazTable {
    lazy val m = db(name)
    type CursorType = m.CursorType

    def exists = db.collectionExists(name)
    def drop = db.getCollection(name).drop
    def size = {
      m.size
    }
    def count(pairs: Map[String, Any]) = m.count(pairs)

    def +=(o: DBObject) = {
      m += o
    }

    def +=(pairs: Map[String, Any]):Unit = dbop("create " + name + " " + pairs.mkString(",")) {
      val x: DBObject = pairs
      m += x
    }

    def findAll() = dbop("findAll " + name) {
      m.find()
    }

    def find(pairs: Map[String, Any]) = dbop("find " + name + " " + pairs.mkString(",")) {
      val x: DBObject = pairs
      retry (1) {
        m.find(x)
      }
    }

    def findOne(pairs: Map[String, Any]) : Option[DBObject] = dbop("findOne " + name + " " + pairs.mkString(",")) {
      val x: DBObject = pairs
      retry (1) {
        m.findOne(x)
      }
    }

    def remove(pairs: Map[String, Any]):Unit = dbop("remove " + name + " " + pairs.mkString(",")) {
      m.remove(pairs)
    }

    def update(pairs: Map[String, Any], o: DBObject):Unit = dbop("update " + name + " " + pairs.mkString(",")) {
      m.update(pairs, o)
    }

    def save(o:DBObject):Unit = dbop("save " + name + o.mkString) {
      m.save(o)
    }
  }

  /** generic retry of idempotent operations (find/read)
    *
    * @param i how many times to retry
    * @param f the function to retry
    * @tparam T whatever return type of f
    * @return
    */
  def retry[T] (i:Int) (f: => T) : T = {
    iretry (i) (f) (None)
  }

  private def iretry[T] (i:Int) (f: => T) (ex:Option[Throwable] = None) : T = {
    if(i <= 0)
      throw ex.map { e=>
        new RuntimeException(e)
      } getOrElse {
        new IllegalArgumentException("can't retry zero times")
      }
    val res = Try {
        f
      } recover {
        case e : com.mongodb.MongoException.Network => {
          Thread.sleep(500)
          iretry (i-1) (f) (Some(e))
        }
        case x@_ => iretry (0) (f) (Some(x))
      }
    res.get
  }
}

/** derive from here and list in upgrades above */
abstract class UpgradeDb {
  def upgrade(db: MongoDB): Unit
  def withDb(d: MongoCollection)(f: MongoCollection => Unit) { f(d) }
}

