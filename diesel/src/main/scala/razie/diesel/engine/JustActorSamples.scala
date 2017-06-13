/**
  * ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package razie.diesel.engine

import akka.actor.{Actor, Props}
import play.libs.Akka

object Sample {

  case class MsgNotify()
  case class MsgFindOrder(orderId: String)
  case class MsgOrderFound(order: {def accountId: String})
  case class MsgFindAccount(accountId: String)
  case class MsgAccountFound(account: {def email:String})
  case class MsgSend(email: String, message: String)

  class SomeDbActor extends Actor {
    def receive = { case _ => {} }
  }
  class SomeEmailActor extends Actor {
    def receive = { case _ => {} }
  }

  class MyActor(orderId:String, notification:String) extends Actor {
    val db    = Akka.system.actorOf(Props(new SomeDbActor()))
    val email = Akka.system.actorOf(Props(new SomeEmailActor()))

    def receive = {
      case MsgNotify => db ! MsgFindOrder(orderId)
      case MsgOrderFound(order) => db ! MsgFindAccount(order.accountId)
      case MsgAccountFound(account) => email ! MsgSend(account.email, notification)
    }
  }

  def notify(orderId: String, notification: String) = {
    val a = Akka.system.actorOf(Props(new MyActor(orderId, notification)))
    a ! new MsgNotify
  }

}

object Sample2 {
  val notificationStream : Stream[(String,String)] = Stream.empty
  object db {
    def findOrder(id:String) : Stream[{def accountId:String}] = Stream.empty
    def findAccount(id:String) : Stream[{def email:String}] = Stream.empty
    def updateAccount(id:Any) : Stream[Boolean] = Stream.empty
    def updateOrder(id:Any) : Stream[Boolean] = Stream.empty
  }
  object email {
    def send(id:String,n:String) : Stream[Boolean] = Stream.empty
  }
  notificationStream.flatMap { tuple=>
    db.findOrder(tuple._1).map(o => (o, tuple._2))
  }.flatMap { tuple=>
    db.updateOrder(tuple._1)
    db.findAccount(tuple._1.accountId).map(o => (o, tuple._2))
  }.flatMap { tuple=>
    db.updateAccount(tuple._1)
    email.send(tuple._1.email, tuple._2);
  }
}

object Interview1 {
  // curry
  def f1(a:Int, b:Int) = a+b
  def f2(a:Int) = f1(a, 4)
  val f3 : Function[Int,Int] = f1(_, 4)
  val gs: Array[String] = new Array[String](3)
  gs(1) = "hah"

  //extractors with no vars use Boolean instead of Option

}

