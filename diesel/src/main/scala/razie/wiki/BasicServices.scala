/**
  * ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package razie.wiki

/** central point of customization - aka service registry
  *
  * todo use some proper injection pattern - this is not MT-safe
  *
  * right now this is setup in Global and different Module(s), upon startup
  */
object BasicServices {

  private var handlers: EventProcessor = new NeverReally

  /** initialize the event processor */
  def initCqrs(al: EventProcessor) = {
    handlers = al
  }

  /** CQRS dispatcher */
  def !(a: Any) = {
    handlers ! a
  }

}

/** this is a generic event and/or task dispatcher - I simply ran out of names...
  *
  * some of the evens are audits, some are entity notifications that spread through the cluster
  *
  * some are just stuff to do later.
  */
trait EventProcessor {
  /** execute work request later */
  def !(a: Any)
}

/** stub implementation - does nothing */
class NeverReally extends EventProcessor {
  def !(a: Any) {}
}
