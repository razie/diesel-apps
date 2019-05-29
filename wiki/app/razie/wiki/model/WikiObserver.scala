/**
 * ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.model

import com.novus.salat.annotations.Ignore
import controllers.{IgnoreErrors, VErrors}

import scala.collection.mutable.ListBuffer

/** just a syntax marker for events -
  *
  * find usages to find all types of events that are sent through the handlers
  * */
trait WikiEventBase {
  def node : String
}

/** an event that the configuration changed */
case class WikiConfigChanged (val node:String="") extends WikiEventBase

/** a generic event refering to an entity
  *
  * @param action what happened to it - use constants below. Extensions must start with prefiex per example
  * @param cls the class, SimpleName - must be understood by processors
  * @param id some form of the entity ID recognized by consumers
  * @param entity the entity, if local on this node and still existing - ignored for serialization
  * @param oldEntity the old entity, if local and entity changed - ignored for serialization
  * @param oldId optionally, if entity was renamed or moved or something, the old id
  * @tparam A the entity type
  */
case class WikiEvent[A] (
  action            :String,
  cls               :String,
  id                :String,
  @Ignore entity    :Option[A]=None,
  @Ignore oldEntity :Option[A]=None,
  oldId             :Option[String]=None,
  node              :String = ""
  ) extends WikiEventBase

/** some constants */
object WikiEvent {
  final val CREATE = "create" // DO NOT CHANGE
  final val UPDATE = "update" // DO NOT CHANGE
  final val DELETE = "delete" // DO NOT CHANGE

  final val MY_SCREWY_UPDATE = "update.screwy"
}

/**
 * implement to get before/after notifications of certain events
 */
trait WikiObserver {
  /** before is invoked before the event and can block the occurence of the event */
  def before(event: WikiEventBase)(implicit errCollector: VErrors = IgnoreErrors): Boolean = { true }
  /** invoked after the event, different cluster node perhaps */
  def after(event: WikiEventBase)(implicit errCollector: VErrors = IgnoreErrors): Unit = { }
}

/** listen to and observe wiki entities being updated
  *
  * todo this is not thread safe
  * */
object WikiObservers {
  val notifieds = new ListBuffer[WikiObserver]()

  def add(n: WikiObserver) = notifieds append n

  /** add an event handler
    *
    * Events are everything of interest:
    * - most events are derived from WikiEventBase
    * - wiki topic changes
    * - configuration changes
    * - etc
    *
    * Your handler needs to be prepared to handle the events on separte threads.
    */
  def mini(upd: PartialFunction[WikiEventBase, Unit]) = {
    add(new WikiObserver {
      override def after(event: WikiEventBase)(implicit errCollector: VErrors = IgnoreErrors): Unit = {
        if (upd.isDefinedAt(event)) upd(event)
      }
    })
  }

  def before(event: WikiEventBase)(implicit errCollector: VErrors = IgnoreErrors): Boolean = {
    notifieds.foldLeft(true)((x, y) => x && y.before(event)(errCollector))
  }
  def after(event: WikiEventBase)(implicit errCollector: VErrors = IgnoreErrors): Unit = {
    notifieds foreach (_.after(event)(errCollector))
  }
}

