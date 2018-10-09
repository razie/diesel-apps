package razie.diesel.dom

import razie.diesel.dom.RDOM._
import razie.tconf.DSpecInventory

import scala.collection.mutable

/**
  * a domain plugin - can adapt a domain to an external implementation
  *
  * name is the type of connector and conn is the actual connection for this instance
  */
trait RDomainPlugin {
  def name: String
  def conn: String

  /** import domain from external */
  def canImportDomain: Boolean = false

  def importDomain: RDomain = ???

  // todo syncDomain into external too ?

  /** html for the supported actions */
  def htmlActions(elem: DE): String

  /**
    * do an action on some domain entity (explore, browse etc)
    *
    * @param r           the domain
    * @param action      the action to execute
    * @param completeUri the entire URL called (use it to get host/port etc)
    * @param epath       id of the entity
    * @return
    */
  def doAction(r: RDomain, action: String, completeUri: String, epath: String): String

  def mkInstance(realm: String, wi: DSpecInventory): List[RDomainPlugin] = Nil

  def findByRef(dom: RDomain, epath: String, collectRefs: Option[mutable.HashMap[String, String]] = None): Option[O] = None

  def findByQuery(dom: RDomain, epath: String, collectRefs: Option[mutable.HashMap[String, String]] = None): List[O] = Nil
}

/** some helpers */
object RDomainPlugins {
  val pluginFactories: List[RDomainPlugin] = new CRMRDomainPlugin :: new DefaultRDomainPlugin :: Nil

  var plugins : String => List[RDomainPlugin] = (x => Nil) // you must provide factory

  def htmlActions(realm:String, c: DE) = {
    RDomainPlugins.plugins(realm).foldLeft("")((a, b) => a + (if (a != "") " <b>|</b> " else "") + b.htmlActions(c))
  }
}

class DefaultRDomainPlugin (override val conn:String="all") extends RDomainPlugin {
  override def name = "diesel"

  // just replicate myself
  override def mkInstance (realm:String, wi:DSpecInventory) : List[RDomainPlugin] = {
    List(new DefaultRDomainPlugin(realm))
  }

  /** html for the supported actions */
  def htmlActions(elem: DE): String = {
    elem match {
      case c: C => {
        def mkList = s"""<a href="/diesel/list2/${c.name}">list</a>"""

        // todo delegate decision to tconf domain - when domain is refactored into tconf
        def mkNew =
          if ("User" != name && "WikiLink" != name)
          //todo move to RDomain
          // if (ctx.we.exists(w => WikiDomain.canCreateNew(w.specPath.realm.mkString, name)))
            s""" <a href="/doe/diesel/create/${c.name}">new</a>"""
          else
            ""

        s"$mkList | $mkNew"
      }

      case _ => "?"
    }
  }

  def doAction(r: RDomain, action: String, uri: String, epath: String) = ???
}


