/**
 * ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.admin

import com.novus.salat.annotations.Ignore
import razie.wiki.util.{IgnoreErrors, VErrors}
import scala.collection.mutable.ListBuffer

/** a generic event refering to an entity
  *
  * @param action what happened to it - use constants below. Extensions must start with prefiex per example
  * @param cls the class, SimpleName - must be understood by processors
  * @param id some form of ID recognized by consumers
  * @param entity the entity, if local on this node and still existing - ignored for serialization
  * @param oldEntity the old entity, if local and entity changed - ignored for serialization
  * @param oldId optionally, if entity was renamed or moved or something, the old id
  * @tparam A the entity type
  */
case class WikiEvent[A] (action:String, cls:String, id:String, @Ignore entity:Option[A]=None, @Ignore oldEntity:Option[A]=None, oldId:Option[String]=None)

/** some constants */
object WikiEvent {
  final val CREATE = "create" // DO NOT CHANGE
  final val UPDATE = "update" // DO NOT CHANGE
  final val DELETE = "delete" // DO NOT CHANGE

  final val MY_SCREWY_UPDATE = "update.screwy"
}

/**
 * simple notification observers attempt - should really move to an actor implementation
  *
  * todo this is not thread safe
  */
trait WikiObserver {
  def before[A](event: WikiEvent[A])(implicit errCollector: VErrors = IgnoreErrors): Boolean = { true }
  def after[A](event: WikiEvent[A])(implicit errCollector: VErrors = IgnoreErrors): Unit = { }
}

/** listen to and observe wiki entities being updated */
object WikiObservers {
  val notifieds = new ListBuffer[WikiObserver]()

  def add(n: WikiObserver) = notifieds append n

  def mini(upd: PartialFunction[WikiEvent[_], Unit]) = {
    add(new WikiObserver {
      override def after[A](event: WikiEvent[A])(implicit errCollector: VErrors = IgnoreErrors): Unit = {
        if (upd.isDefinedAt(event)) upd(event)
      }
    })
  }

  def before[A](event: WikiEvent[A])(implicit errCollector: VErrors = IgnoreErrors): Boolean = {
    notifieds.foldLeft(true)((x, y) => x && y.before(event)(errCollector))
  }
  def after[A](event: WikiEvent[A])(implicit errCollector: VErrors = IgnoreErrors): Unit = {
    val xx = notifieds.map(_.getClass.getCanonicalName)
    notifieds foreach (_.after(event)(errCollector))
  }
}

