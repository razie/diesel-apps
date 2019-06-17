/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.model

import razie.diesel.dom.WikiDomain
import razie.hosting.WikiReactors
import razie.wiki.util.DslProps

/** a hosted wiki instance, i.e. independent hosted website.
  *
  * It has its own index, domain and is independent of other wikis
  *
  * It has its own users and admins/mods etc
  *
  * Wikis can mixin other wikis - linearized multiple inheritance.
  */
trait Reactor {
  def realm:String
  def fallBacks:List[Reactor]
  def we:Option[WikiEntry]

  def wiki   : WikiInst
  def domain : WikiDomain

  val mixins : Mixins[Reactor]
  def club :Option[WikiEntry]

  def userRoles :List[String]
  def adminEmails :List[String]

  // list of super reactors linearized
  val supers : Array[String]

  /** Admin:UserHome if user or Admin:Home or Reactor:realm if nothing else is defined */
  def mainPage(au:Option[WikiUser]) : WID

  def websiteProps : DslProps

  //todo fallback also in real time to rk, per prop
  // todo listen to updates and reload
  def props : DslProps
}

/** a hosted wiki instance, i.e. independent hosted website.
  *
  * It has its own index, domain and is independent of other wikis
  *
  * It has its own users and admins/mods etc
  *
  * Wikis can mixin other wikis - linearized multiple inheritance.
  */
abstract class ReactorImpl (val realm:String, val fallBacks:List[Reactor] = Nil, val we:Option[WikiEntry]) extends Reactor {
  val mixins = new Mixins[Reactor](fallBacks)
  lazy val club = props.wprop("club").flatMap(Wikis.find)

  lazy val userRoles = websiteProps.prop("userRoles").toList.flatMap(_.split(','))
  lazy val adminEmails = websiteProps.prop("adminEmails").toList.flatMap(_.split(','))

  // list of super reactors linearized
  val supers : Array[String] = {
    if(realm == WikiReactors.RK) Array(WikiReactors.RK)
    else mixins.flatten.map(_.realm).toArray
  }

  /** Admin:UserHome if user or Admin:Home or Reactor:realm if nothing else is defined */
  def mainPage(au:Option[WikiUser]) = {
    def dflt = WID("Admin", "UserHome").r(realm).page.map(_.wid)
//    val p = au.flatMap {user=>
//      if(adminEmails.contains(Dec(user.email)))
//        WID("Admin", "AdminHome").r(realm).page.map(_.wid) orElse dflt
//      else
//        club.flatMap(c=> user.myPages(c.realm, "Club").find(_.uwid.id == c._id)).flatMap {uw=>
//          WID("Admin", uw.role+"Home").r(realm).page.map(_.wid) orElse dflt
//        } orElse
//          dflt
      val p = au.flatMap {user=>
        dflt
    } orElse WID("Admin", "Home").r(realm).page.map(_.wid) getOrElse WID("Reactor", realm).r(realm)
    p
  }

  private def sectionProps(section:String) = {
    we.orElse(WID("Reactor", realm).r(realm).page).map{p=>
      new DslProps(Some(p), section)
    } getOrElse
      WikiReactors.fallbackProps
  }

  /* deprecated - use props */
  lazy val websiteProps = sectionProps("website,properties")// :: sectionProps("properties")

  //todo fallback also in real time to rk, per prop
  // todo listen to updates and reload
  lazy val props = sectionProps ("properties,website") //:: sectionProps("website")

}


