/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.admin

/**
 * many operations are audited - this is strait in the log files, as well as the logdb() audit facility.
 * Audited events are supposed to end up somewhere special or even notify someone etc
 *
 * in my RK, I simply log them in a database, for review - you could do the same or ignore them or syslogd tham etc
 *
 * there's a Audit default implementation here somehwer
 */
trait Auditor {

  /** log a db opreation - this method you need to provide */
  def logdb(what: String, details: Any*): String

  /** log a db operation with a link to the thing being audited */
  def logdbWithLink(what: String, link: String, details: Any*): String
}

