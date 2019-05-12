/**
 *  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.dom

import scala.collection.mutable

case class DomContextEntry[T] (role:String, value:T, link:Option[String])

/** a context - somewhat like a current request context
  *
  * - you can put objects in the context
  * - it interacts with the domain model and data sources
  *
  * todo this appears unused
  */
class DomContext (
  realm:String,
  context:String,
  userId:Option[String]
  ) {

  /** store of objects "in context" */
  val objects = new mutable.HashMap[String,DomContextEntry[_]]()

  def put[T] (domContextEntry: DomContextEntry[T]) = {
    objects.put(domContextEntry.role, domContextEntry)
  }

  override def toString = {
    objects.mkString(" | ")
  }

  def toj : Map[String,Any] = {
    Map (
      "class" -> "DomContext",
      "objects" -> razie.js.tojson(objects.toMap)
    )
  }

  def tojchildren (kids : List[DomAst]) : List[Any] =
      kids.filter(k=> !AstKinds.shouldIgnore(k.kind)).flatMap{k=>
          List(k.toj)
      }

  /** this is the big kahuna */
  def find (key:String, id:String) : Option[DomContextEntry[_]] = {
    objects.get(key)
  }
}

object DomContext {
  final val CTX_DEFAULT = "default"

  val map = new mutable.HashMap[String, DomContext]()

  def apply (realm:String, ctx:String, userId:Option[String]) = {
    new DomContext(realm, ctx, userId)
  }
}

