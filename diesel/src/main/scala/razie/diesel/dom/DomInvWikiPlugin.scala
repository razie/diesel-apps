/** ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.dom

import razie.diesel.Diesel
import razie.diesel.dom.RDOM._
import razie.diesel.engine.nodes.EMsg
import razie.tconf.{DSpecInventory, FullSpecRef, SpecRef}

/** default inventory for wiki defined classes */
class DomInvWikiPlugin(val specInv: DSpecInventory, val realm: String, override val conn: String = "default",
                       ienv: String) extends
    DomInventory {
  override def name = "wiki"

  var env = ienv

  override def isDefinedFor(realm: String, c: C): Boolean = {
    c.stereotypes.contains(razie.diesel.dom.WikiDomain.WIKI_CAT)
  }

  // just replicate myself
  /**
    * make instance for specific realm - read the specs and if any applicable, create a plugin and initialize it's
    * connections
    * Each plugin is responsible to configure itself, there's no standard -
    * usually they're added as ReactorMod, see the OdataCrmPlugin for example
    *
    * @param realm  the realm this is for
    * @param wi     - spec inventory, use it to lookup configuration topics, diesel plugin topics etc
    * @param iprops initial properties, when created via diesel message
    */
  override def mkInstance(realm: String, env: String, wi: DSpecInventory, newName: String, iprops: Map[String,
      String]): List[DomInventory] = {
    List(new DomInvWikiPlugin(wi, realm, "default", env))
  }

  override def connect(dom: RDomain, env: String): Either[P, EMsg] =
    Left(P.of(Diesel.PAYLOAD, "DomInvWikiPlugin connect ok"))

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


  def doAction(r: RDomain, conn: String, action: String, uri: String, epath: String) = {
    throw new IllegalArgumentException(s"DomInventory not defined for $uri and $epath")
  }
}

