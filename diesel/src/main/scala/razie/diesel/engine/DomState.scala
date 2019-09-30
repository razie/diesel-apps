/**   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.engine

/** the states of a node */
object DomState {
  final val INIT="initial.init" // new node
  final val STARTED="exec.started" // is executing now
  final val DONE="final.done" // done
  final val CANCEL="final.cancelled" // done
  final val SKIPPED="final.skipped" // skipped
  final val LATER="exec.later" // queued up somewhere for later
  final val ASYNC="exec.async" // started but will complete itself later
  final val DEPENDENT="exec.depy" // waiting on another task

  def inProgress(s:String) = s startsWith "exec."
  def isDone(s:String) = s startsWith "final."
}
