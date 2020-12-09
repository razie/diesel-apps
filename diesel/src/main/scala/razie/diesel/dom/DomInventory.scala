/** ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.dom

import org.json.JSONObject
import razie.diesel.Diesel
import razie.diesel.dom.RDOM._
import razie.diesel.engine.nodes.EMsg
import razie.diesel.expr.ECtx
import razie.diesel.model.{DieselMsg, DieselTarget}
import razie.tconf.{DSpecInventory, FullSpecRef, SpecRef}
import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration.Duration

// todo study with cmd pattern - should we do actors?
class DIBase()
case class DIConnect(dom:RDomain, env:String, ctx: ECtx) extends DIBase
case class DITestConnection(dom:RDomain, env:String, ctx: ECtx) extends DIBase

/**
  * a domain plugin - can adapt a domain to an external implementation
  *
  * name is the type of connector and conn is the actual connection for this instance
  *
  * for instance (name=CRM, conn=CRM1) (name=CRM, conn=CRM2)
  *
  * Any kind of connection pooling is done inside the plugin - outside they are referenced by name
  */
trait DomInventory {
  def name: String
  def realm: String
  def env: String
  def conn: String

  /** import domain from external */
  def canImportDomain: Boolean = false

  /** import a subdomain from external model - if this can import it, there's a special operation to import it
    *
    * which you can call manually to sync the definitions
    *
    * this imported definition should be saved and that will add it to the domain
    *
    * standard topic for imported domains: DomNameDomain
    */
  def importDomain: RDomain = ???

  /** test this connection */
  def testConnection(dom: RDomain, epath:String): Either[P,EMsg] = ???

  def connect(dom:RDomain, env:String): Either[P,EMsg] = ???

  /** create an element */
  def upsert(dom: RDomain, ref: FullSpecRef, asset:DieselAsset[_]) : Either[Option[DieselAsset[_]], EMsg] = ???

  /** list all elements of class */
  def listAll(dom: RDomain, ref: FullSpecRef, collectRefs: Option[mutable.HashMap[String, String]] = None)
  : Either[List[DieselAsset[_]], EMsg] = ???

  // todo syncDomain into external too ?

  /**
    * make instance for specific realm - read the specs and if any applicable, create a plugin and initialize it's
    * connections
    * Each plugin is responsible to configure itself, there's no standard -
    * usually they're added as ReactorMod, see the OdataCrmPlugin for example
    *
    * @param realm the realm this is for
    * @param env which environment
    * @param wi - spec inventory, use it to lookup configuration topics, diesel plugin topics etc
    * @param iprops initial properties, when created via diesel message
    */
  def mkInstance(realm: String, env:String, wi: DSpecInventory, newName:String, iprops: Map[String, String] = Map.empty): List[DomInventory] = Nil

  /**
    * can this support the class? You can look at it's annotations etc
    *
    * You can either use the `diesel.inv.register` message or annotate your known classes withe a
    * specific annotation like `odata.name` etc
    */
  def isDefinedFor(realm: String, c: C): Boolean = {
    DomInventories.invRegistry.get(c.name).exists(_ == this.name)
  }

  /** find an element by ref */
  def findByRef(dom: RDomain, ref: FullSpecRef, collectRefs: Option[mutable.HashMap[String, String]] = None)
  : Either[Option[DieselAsset[_]], EMsg] = ???

  /** find an element by query */
  def findByQuery(dom: RDomain, ref: FullSpecRef, epath: String, collectRefs: Option[mutable.HashMap[String, String]]
  = None): List[DieselAsset[_]] = Nil

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
  def doAction(r: RDomain, conn: String, action: String, completeUri: String, epath: String): String
}

/** some helpers */
object DomInventories extends razie.Logging {

  /**
    * list all
    */
  private def runMsg(msg:DieselMsg): Option[P] = {
    val fut = msg.toMsgString.startMsg

    // get timeout max from realm settings: paid gets more etc
    val res = Await.result(fut, Duration.create(30, "seconds"))
    res.get(Diesel.PAYLOAD).map(_.asInstanceOf[P])
  }

  /**
    * list all
    */
  private def runMsg(realm:String, m: EMsg): Option[P] = {
    runMsg(new DieselMsg(m, DieselTarget.ENV(realm)))
  }

  /**
    * list all
    */
  private def runMsg(realm:String, e: String, a: String, parms: Map[String, Any]): Option[P] = {
    runMsg(DieselMsg(e, a, parms, DieselTarget.ENV(realm)))
  }

  /** for synchornous people */
  def resolve(e:Either[P,EMsg]) : P = {
    // resolve EMrg's parameters in an empty context and run it and await?
    e.fold (
      p=> p,
      m=> runMsg("?", m).getOrElse(P.undefined(Diesel.PAYLOAD))
    )
  }

  /** for synchornous people */
  def resolve(e:Either[Option[DieselAsset[_]],EMsg]) : Option[DieselAsset[_]] = {
    // resolve EMrg's parameters in an empty context and run it and await?
    e.fold (
      p=> p,
      m=> {
        val p = runMsg("?", m)
        if(p.isEmpty || !p.get.isOfType(WTypes.wt.JSON)) {
          log("sub-flow return nothing or not an object - so no asset found!")
          None
        } else {
          val j = p.get.calculatedTypedValue(ECtx.empty).asJson
          val r = j.get("assetRef")
              .filter(_.isInstanceOf[Map[String, _]])
              .map(_.asInstanceOf[Map[String, _]])
              .map(SpecRef.fromJson)

            Some(new DieselAsset[P] (
              ref = r.get,
              value = p.get
            ))
        }
      }
    )
  }

  // todo add the CRMR plugin only if the reactor has one...
  var pluginFactories: List[DomInventory] =
    new OdataCRMDomInventory ::
        new DieselRulesInventory() ::
        new DefaultRDomainPlugin(null, "", "", "") ::
        Nil

  /** register (class, inventory) */
  var invRegistry = new TrieMap[String, String]()

  // you must provide factory and the domain when loading the realm will instantiate all plugins and connections

  /** find the right plugin by name and conn */
  def getPlugin(realm:String, inv:String, conn:String) : Option[DomInventory] = {
    val dom = WikiDomain(realm)
    val list = dom.findPlugins(inv)
    val p = (if(conn.length > 0) list.filter(_.conn == conn) else list).headOption
    trace(s"  Found inv $p")
    p
  }

  /** find the right plugin by name and conn */
  def getPluginForClass(realm:String, cls:DE, conn:String) : Option[DomInventory] = {
    val dom = WikiDomain(realm)
    var list = dom.findPluginsForClass(cls)
    if(list.isEmpty) {
      invRegistry.get(cls.asInstanceOf[C].name).foreach(inv =>
          list = dom.findPlugins(inv, conn)
      )
    }
    val p = (if(conn.length > 0) list.filter(_.conn == conn) else list).headOption
    trace(s"  Found inv $p")
    p
  }

  /** aggregate applicable actions on element in realm's plugins */
  def htmlActions(realm: String, c: DE) = {
    WikiDomain(realm).findPluginsForClass(c).foldLeft("")((a, b) => a + (if (a != "") " <b>|</b> " else "") + b.htmlActions(c))
  }

  /** find an element by ref */
  def findByRef(ref: FullSpecRef, collectRefs: Option[mutable.HashMap[String, String]] = None)
  : Option[DieselAsset[_]] = {
    trace(s"findByRef $ref")
    val dom = WikiDomain(ref.realm)
    val p = getPlugin(ref.realm, ref.inventory, ref.conn)
    trace(s"  Found inv $p")
//    val o = p.flatMap(_.findByRef(dom.rdom, ref, collectRefs))
//    trace(s"  Found obj $o")
//    o
  None}

  /** find an element by query
    *
    * @param ref contains the plugin,conn,class, no ID
    * @param epath
    * @param collectRefs
    * @return
    */
  def findByQuery(ref: FullSpecRef, epath: String, collectRefs: Option[mutable.HashMap[String, String]]
  = None): List[DieselAsset[_]] = {
    val dom = WikiDomain(ref.realm)
    val p = dom.findPlugins(ref.inventory).headOption
    val o = p.toList.flatMap(_.findByQuery(dom.rdom, ref, epath, collectRefs))
    o
  }

  /** turn a json value into a nice object, merge with class def and mark refs etc */
  def oFromJ (name:String, j:JSONObject, c:C, invClsName:String, filterAttrs:Array[String]) = {

    // move parms containing name/desc to the top of the list
    val parmNames = j.keySet
        .toArray
        .toList
        .map(_.toString)
        .filter(n=> !n.startsWith("@"))
        .filter(n=> !filterAttrs.contains(n))
    //      .sorted
    val a1 = parmNames.filter(n=> n.contains("name") || n.contains("key"))
    val a2 = parmNames.filter(n=> n.contains("description") || n.contains("code"))
    val b = parmNames.filterNot(n=> n.contains ("name") || n.contains("description") || n.contains("code"))

    val parms = (a1 ::: a2 ::: b)
        .map {k=>
          val value = j.get(k).toString
          val kn = k.toString
          val oname = invClsName

          c.parms.find(_.name == kn).map {cp=>
            cp.copy(dflt = value.toString) // todo add PValue
          } getOrElse {
            // key refs
            if(kn.startsWith("_") && kn.endsWith(("_value")) ) {
              val PAT="""_(.+)_value""".r
              val PAT(n) = kn

              c.parms.find(_.name == n).map {cpk=>
                cpk.copy(dflt = value.toString) // todo add PValue
              } getOrElse {
                P(kn, value)
              }
            } else
              P(kn, value)
          }
        }

    O(name, c.name, parms)
  }

}

/** default inventory for wiki defined classes */
class DefaultRDomainPlugin(val specInv: DSpecInventory, val realm: String, override val conn: String = "default", ienv:String) extends
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
    * @param realm the realm this is for
    * @param wi - spec inventory, use it to lookup configuration topics, diesel plugin topics etc
    * @param iprops initial properties, when created via diesel message
    */
  override def mkInstance(realm: String, env:String, wi: DSpecInventory, newName:String, iprops: Map[String, String]): List[DomInventory] = {
    List(new DefaultRDomainPlugin(wi, realm, "default", env))
  }

  override def connect(dom: RDomain, env: String): Either[P, EMsg] =
    Left(P(Diesel.PAYLOAD, "ok"))

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

  def doAction(r: RDomain, conn: String, action: String, uri: String, epath: String) = ???
}

