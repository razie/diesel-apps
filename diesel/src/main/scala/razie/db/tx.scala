/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.db

import org.bson.types.ObjectId
import razie.{cdebug, clog}
import com.mongodb.casbah.Imports._
import com.novus.salat._
import com.novus.salat.annotations._

/** playing with "transactions" i.e. a set of related db ops
  *
  * they are audited and timed in the log automatically
  *
  * otherwise, this is not really useful right now
  */
class Txn (val name: String, val user:String, val xid:String = System.currentTimeMillis().toString) {
  clog << "DB.TX.START "+name

  def add(s: String, ss: String) {}

//  def map[B] (f:A=>B) : Txn[B]
//  def flatMap[B] (f:A=>Txn[B]) : Txn[B]

  def commit {
    clog << "DB.TX.COMMIT "+name
  }
  def rollback {
    clog << "DB.TX.ROLLBACK... NOT! "+name
  }

  def id = name+"-"+xid
}

/** main helper for "transactions"
  *
  * the only use right now is as a marker in the code, for related sets of DB udpates
  *
  * since Mongo doens't do transactions, that's all this is for now
  *
  * todo it seems fairly simple to implement rollbacks with a JSON document commit log, at some point.
  */
object tx {
  def apply[A] (name: String, user:String) (f: Txn => A) : A = {
    val txn = new Txn(name, user)
    val res : A = try {
      f(txn)
    } catch {
      case t: Throwable => {
        // can't really log in db while handling a db exception, can i?
        clog << ("ERR_TXN_FAILED", txn.name)
        txn.rollback
        throw new RuntimeException(t)
      }
    }
    txn.commit
    res
  }

  def auto = local ("auto", "?")
  def local (name:String, user:String) : Txn = new Txn("local"+name, user)
  def t (name:String, user:String) : Txn = new Txn(name, user)
  implicit def txn = auto
}

/** wrap all db operation in this to get logging, timing and stats */
object dbop {
  def apply[A](f: => A): A = this.apply("-")(f)

  def apply[A](name: String)(f: => A): A = {
    val ba = new RBeforeAft(name)
    cdebug << s"dbop.before for ${ba.name}"
    val res = f
    val t2 = System.currentTimeMillis
    val debinf = res match {
      case None => "None"
      case Some(_) => "Some"
      case x@_ => x.getClass.getName
    }
    cdebug << s"dbop.after ${t2 - ba.t1} millis for ${ba.name} res: $debinf"
    res
  }

  private class RBeforeAft(val name: String, val t1: Long = System.currentTimeMillis)
}


