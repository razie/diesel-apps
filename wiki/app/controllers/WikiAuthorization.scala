/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package controllers

import razie.wiki.model._

/** Authorization: minimum authorization functionality required
  *
  * provide your own at startup, in Services.wikiAuth
  */
trait WikiAuthorization {

  /** can user see a topic with the given properties? */
  def isVisible(u: Option[WikiUser], props: Map[String, String], visibility: String = "visibility", we: Option[WikiEntry]=None)(implicit errCollector: VErrors = IgnoreErrors): Boolean

  /**
   * can the user see the topic - a little more checks than isVisibile - this is the one to use
   *
   * can pass admin.IgnoreErrors as an errCollector
   *
   * @return Some(true) or None - an option so you can use in for comprehensions. None is the same as false
   */
  def canSee(wid: WID, au: Option[WikiUser], w: Option[WikiEntry])(implicit errCollector: VErrors): Option[Boolean]

  /**
   * can the user edit the topic
   *
   *  can pass admin.IgnoreErrors as an errCollector
   *
   * @return Some(true) or None - an option so you can use in for comprehensions. None is the same as false
   */
  def canEdit(wid: WID, u: Option[WikiUser], w: Option[WikiEntry], props: Option[Map[String, String]] = None)(implicit errCollector: VErrors): Option[Boolean]

  /** extract wvis (edit permissions) prop from wiki */
  protected def wvis(props: Option[Map[String, String]]): Option[String] =
    props.flatMap(p => p.get("wvis").orElse(p.get("visibility"))).map(_.asInstanceOf[String])
}

/** stub sample authorization implementation */
class NoWikiAuthorization extends WikiAuthorization {

  /** can user see a topic with the given properties? */
  def isVisible(u: Option[WikiUser], props: Map[String, String], visibility: String = "visibility", we: Option[WikiEntry]=None)(implicit errCollector: VErrors = IgnoreErrors): Boolean = {
    true
  }

  /**
   * can the user see the topic - a little more checks than isVisibile - this is the one to use
   *
   * can pass admin.IgnoreErrors as an errCollector
   */
  def canSee(wid: WID, au: Option[WikiUser], w: Option[WikiEntry])(implicit errCollector: VErrors): Option[Boolean] = {
    Some(true)
  }

  /**
   * can the user edit the topic
   *
   *  can pass admin.IgnoreErrors as an errCollector
   */
  def canEdit(wid: WID, u: Option[WikiUser], w: Option[WikiEntry], props: Option[Map[String, String]] = None)(implicit errCollector: VErrors): Option[Boolean] = {
    Some(true)
  }
}
