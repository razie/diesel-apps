package razie.diesel.dom

import razie.diesel.dom.RDOM._

/**
  * a domain plugin - can adapt a domain to an external implementation
  */
trait RDomainPlugin {
  def name:String

  /** import domain from external */
  def canImportDomain : Boolean = false
  def importDomain : RDomain = ???

  // todo syncDomain into external too ?

  /** html for the supported actions */
  def htmlActions (elem : DE) : String
}

/** some helpers */
object RDomainPlugins {
  val plugins = new CRMRDomainPlugin :: new DefaultRDomainPlugin :: Nil
}

class DefaultRDomainPlugin extends RDomainPlugin {
  override def name = "default"

  /** html for the supported actions */
  def htmlActions (elem : DE) : String = {
    elem match {
      case c : C => {
        def mkList = s"""<a href="/diesel/list2/${c.name}">list</a>"""

        // todo delegate decision to tconf domain - when domain is refactored into tconf
        def mkNew =
          if("User" != name && "WikiLink" != name)
          //todo move to RDomain
          // if (ctx.we.exists(w => WikiDomain.canCreateNew(w.specPath.realm.mkString, name)))
            s""" <a href="/doe/diesel/create/${c.name}">new</a>"""
          else
            ""

        s"$mkList | $mkNew"
      }

      case _ =>  "?"
    }
  }
}

class CRMRDomainPlugin extends RDomainPlugin {
  override def name = "OData"

  final val ODATA_NAME = "odata.name"

  def URL = "https://omniitelyatest.crm3.dynamics.com/api/data/v8.2/"

  /** html for the supported actions */
  def htmlActions (elem : DE) : String = {
    elem match {
      case c : C => {
        val oname = c.props.find(_.name == ODATA_NAME).map(_.calculatedValue(ECtx.empty)).getOrElse(c.name)

        def mkMetA = s"""<a href="$URL/EntityDefinitions(LogicalName='$oname')/Attributes?$$select=LogicalName">attrs</a>"""
        def mkMet = s"""<a href="$URL/EntityDefinitions(LogicalName='$oname')">def</a>"""

        def mkSample = s"""<a href="$URL/${oname}s?$$top=1">sample</a>"""

        s"$mkMet | $mkMetA | $mkSample"
      }

      case _ =>  "?"
    }
  }
}

