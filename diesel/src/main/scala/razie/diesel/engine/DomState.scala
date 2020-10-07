/*  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

/** the states of a node */
object DomState {
  final val INIT = "initial.init" // new node
  final val STARTED = "exec.started" // is executing now
  final val DONE = "final.done" // done
  final val CANCEL = "final.cancelled" // done
  final val SKIPPED = "final.skipped" // skipped
  final val LATER = "exec.later" // queued up somewhere for later
  final val SUSPENDED = "exec.waiting.suspended" // queued up somewhere for later - holds up siblings
  final val DAEMON = "exec.waiting.daemon" // queued up somewhere for later - does not hold up siblings
  final val ASYNC = "exec.async" // started but will complete itself later
  final val DEPENDENT = "exec.depy" // waiting on another task

  final val GUARD_NONE = "no" // had no guard
  final val GUARD_FALSE = "false" // had guard and was false
  final val GUARD_TRUE = "true" // had guard and was true

  def inProgress(s: String) = s startsWith "exec."

  def isDone(s: String) = s startsWith "final."

  def inWaiting(s: String) = s startsWith "exec.waiting."
}
