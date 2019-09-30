/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.db

/** SingletonInstance - temp model for pre-configured singleton instances
  *
  * todo use some injection library of some kind
  *
  * Your final singletons extend this, such as RMongo extends SI[xxx]
  *
  * Singletons are initialized by someone on startup (i.e. injection)
  */
abstract class SI[T >: Null <: AnyRef] (what:String, initial:T = null) {
  private var idb : T = initial

  /** set the instance to use */
  def setInstance (adb:T) = {
    if(idb ne initial)
      throw new IllegalStateException(what+" instance already initialized... ")
    idb = prepInstance(adb)
  }
  def getInstance = {
    if(idb == null)
      throw new IllegalStateException(what+" NOT initialized...")
    idb
  }

  /** overwrite this to prepare your instance i.e. initialize it */
  protected def prepInstance(t:T) : T = {t} // nop
}


