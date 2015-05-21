/**
 * ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.admin

import razie.wiki.util.{IgnoreErrors, VErrors}
import scala.collection.mutable.ListBuffer

/** simple notification observers attempt - should really move to an actor implementation
  *
  * todo this is not thread safe
  */
trait WikiObserver {
  def entityCreateBefore[A](e: A)(implicit errCollector: VErrors = IgnoreErrors): Boolean = { true }
  def entityCreateAfter[A](e: A)(implicit errCollector: VErrors = IgnoreErrors): Unit = {}

  def entityUpdateBefore[A](e: A, what: String)(implicit errCollector: VErrors = IgnoreErrors): Boolean = { true }
  def entityUpdateAfter[A](e: A, what: String)(implicit errCollector: VErrors = IgnoreErrors): Unit = {}
}

/** listen to and observe wiki entities being updated */
object WikiObservers {
  val notifieds = new ListBuffer[WikiObserver]()

  def add(n: WikiObserver) = notifieds append n

  def mini(upd: PartialFunction[Any, Unit]) = {
    add(new WikiObserver {
      override def entityCreateAfter[A](e: A)(implicit errCollector: VErrors = IgnoreErrors): Unit = {
        if (upd.isDefinedAt(e)) upd(e)
      }

      override def entityUpdateAfter[A](e: A, what: String)(implicit errCollector: VErrors = IgnoreErrors): Unit = {
        if (upd.isDefinedAt(e)) upd(e)
      }
    })
  }

  def entityCreateBefore[A](e: A)(implicit errCollector: VErrors = IgnoreErrors): Boolean = {
    notifieds.foldLeft(true)((x, y) => x && y.entityCreateBefore(e)(errCollector))
  }

  def entityCreateAfter[A](e: A)(implicit errCollector: VErrors = IgnoreErrors): Unit = {
    notifieds map (_.entityCreateAfter(e)(errCollector))
  }

  def entityUpdateBefore[A](e: A, what: String)(implicit errCollector: VErrors = IgnoreErrors): Boolean = {
    notifieds.foldLeft(true)((x, y) => x && y.entityUpdateBefore(e, what))
  }

  def entityUpdateAfter[A](e: A, what: String)(implicit errCollector: VErrors = IgnoreErrors): Unit = {
    notifieds map (_.entityUpdateAfter(e, what))
  }
}
