/** ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.dom

import org.json.JSONObject
import razie.diesel.Diesel
import razie.diesel.dom.RDOM.{O, _}
import razie.diesel.engine.DieselException
import razie.diesel.engine.nodes.{EMsg, flattenJson}
import razie.diesel.expr.{DieselExprException, ECtx}
import razie.diesel.model.{DieselMsg, DieselTarget}
import razie.diesel.samples.DomEngineUtils
import razie.tconf.{DSpecInventory, FullSpecRef, SpecRef}
import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration.Duration

/** some helpers */
object DomInventories extends razie.Logging {

  final val RESERVEDP = Array(
    "inventory",
    "connection",
    "class",
    "className",
    "key",
    "section"
  )

  // todo add the CRMR plugin only if the reactor has one...
  var pluginFactories: List[DomInventory] =
    new DomInvOdataCRMPlugin ::
        new DieselRulesInventory() ::
        new DomInvWikiPlugin(null, "", "", "") ::
        Nil

  /** register (class, inventory) - we allow just one per */
  var invRegistry = new TrieMap[String, String]()

  /** register one plugin per class */
  def registerPlugin(realm: String, clsName: String, inv: String) = {
    // todo use realm
    invRegistry.put(clsName, inv)
  }

  // you must provide factory and the domain when loading the realm will instantiate all plugins and connections

  /** find the right plugin by name and conn */
  def getPlugin(realm: String, inv: String, conn: String): Option[DomInventory] = {
    val dom = WikiDomain(realm)
    val list = dom.findPlugins(inv)
    val p = (if (conn.length > 0) list.filter(_.conn == conn) else list).headOption
    trace(s"  Found inv $p")
    p
  }

  /** find the right plugin by name and conn */
  def getPluginForClass(realm: String, cls: DE, conn: String): Option[DomInventory] = {
    val dom = WikiDomain(realm)
    var list = dom.findPluginsForClass(cls)
    if (list.isEmpty) {
      invRegistry.get(cls.asInstanceOf[C].name).foreach(inv =>
        list = dom.findPlugins(inv, conn)
      )
    }
    val p = (if (conn.length > 0) list.filter(_.conn == conn) else list).headOption
    trace(s"  Found inv $p")
    p
  }

  /** aggregate applicable actions on element in realm's plugins */
  def htmlActions(realm: String, c: DE) = {
    val s = WikiDomain(realm)
        .findPluginsForClass(c)
        .foldLeft("")(
          (a, b) => a + (if (a != "") " <b>|</b> " else "") + b.htmlActions(c)
        )

    // todo add an info question mark popup to prompt them to read about inventoryies and assets etc
    if (s.trim.isEmpty) "<small><i><span style=\"color:red\">no inventory registered</span></i></small>"
    else s
  }

  /** find an element by ref */
  def findByRef(ref: FullSpecRef, collectRefs: Option[mutable.HashMap[String, String]] = None): Option[DieselAsset[_]]
  = {
    trace(s"findByRef $ref")
    val dom = WikiDomain(ref.realm)
    val p = getPlugin(ref.realm, ref.inventory, ref.conn)
    trace(s"  Found inv $p")
    val o = p.flatMap(inv =>
      resolve(
        ref.realm,
        ref,
        inv.findByRef(dom.rdom, ref, collectRefs)
      )
    )
//    trace(s"  Found obj $o")
    o
  }

  /** find an element by query
    *
    * @param ref contains the plugin,conn,class, no ID
    * @param epath
    * @param collectRefs
    * @return
    */
  def findByQuery(ref: FullSpecRef, epath: Either[String, collection.Map[String, Any]],
                  from: Long = 0, size: Long = 100, sort: Array[String],
                  countOnly: Boolean = false,
                  collectRefs: Option[mutable.HashMap[String, String]] = None)
  : DIQueryResult = {
    val dom = WikiDomain(ref.realm)
    val p = dom.findPlugins(ref.inventory).headOption
    val o = p.map(inv =>
      resolve(
        ref.realm,
        ref,
        inv.findByQuery(dom.rdom, ref, epath, from, size, sort, countOnly, collectRefs)
      )
    )
    o.getOrElse(DIQueryResult(0))
  }

  /** query all
    *
    * @param ref contains the plugin,conn,class, no ID
    * @param epath
    * @param collectRefs
    * @return
    */
  def listAll(ref: FullSpecRef, start: Long = 0, limit: Long = 100, sort: Array[String], collectRefs: Option[mutable
  .HashMap[String, String]]
  = None): DIQueryResult = {
    val dom = WikiDomain(ref.realm)
    val p = dom.findPlugins(ref.inventory).headOption
    val o = p.map(inv =>
      resolve(
        ref.realm,
        ref,
        inv.listAll(dom.rdom, ref, start, limit, sort, collectRefs)
      )
    )
    o.getOrElse(DIQueryResult(0))
  }

  /** turn a json value into a nice object, merge with class def and mark refs etc */
  def oFromJMap(name: String, j: Map[String, Any], classDef: C, invClsName: String, filterAttrs: Array[String]) = {

    // move parms containing name/desc to the top of the list
    val parmNames = j.keySet
        .toArray
        .toList
        .map(_.toString)
        .filter(n => !n.startsWith("@"))
        .filter(n => !filterAttrs.contains(n))
    //      .sorted
    val a1 = parmNames.filter(n => n.contains("name") || n.contains("key"))
    val a2 = parmNames.filter(n => n.contains("description") || n.contains("code"))
    val b = parmNames.filterNot(
      n => n.contains("key") || n.contains("name") || n.contains("description") || n.contains("code"))

    // augment each parm with ttype from class def
    val parms = (a1 ::: a2 ::: b)
        .map { k =>
          val value = j.get(k).mkString
          val kn = k.toString
          val oname = invClsName

          classDef.parms.find(_.name == kn).map { cp =>
            cp.copy(dflt = value) // todo add PValue
          } getOrElse {
            // todo this is odata remnants...
            if (kn.startsWith("_") && kn.endsWith(("_value"))) {
              val PAT = """_(.+)_value""".r
              val PAT(n) = kn

              classDef.parms.find(_.name == n).map { cpk =>
                cpk.copy(dflt = value) // todo add PValue
              } getOrElse {
                P.of(kn, value)
              }
            } else
              P.of(kn, value)
          }
        }

    O(name, classDef.name, parms)
  }

  /** turn a json value into a nice object, merge with class def and mark refs etc */
  def oFromJ(name: String, j: JSONObject, c: C, invClsName: String, filterAttrs: Array[String]) = {

    // move parms containing name/desc to the top of the list
    val parmNames = j.keySet
        .toArray
        .toList
        .map(_.toString)
        .filter(n => !n.startsWith("@"))
        .filter(n => !filterAttrs.contains(n))
    //      .sorted
    val a1 = parmNames.filter(n => n.contains("name") || n.contains("key"))
    val a2 = parmNames.filter(n => n.contains("description") || n.contains("code"))
    val b = parmNames.filterNot(n => n.contains("name") || n.contains("description") || n.contains("code"))

    val parms = (a1 ::: a2 ::: b)
        .map { k =>
          val value = j.get(k).toString
          val kn = k.toString
          val oname = invClsName

          c.parms.find(_.name == kn).map { cp =>
            cp.copy(dflt = value) // todo add PValue
          } getOrElse {
            // key refs
            if (kn.startsWith("_") && kn.endsWith(("_value"))) {
              val PAT = """_(.+)_value""".r
              val PAT(n) = kn

              c.parms.find(_.name == n).map { cpk =>
                cpk.copy(dflt = value) // todo add PValue
              } getOrElse {
                P.of(kn, value)
              }
            } else
              P.of(kn, value)
          }
        }

    O(name, c.name, parms)
  }

  val CLS_FIELD_VALUE = """([^/]+)/(.+)/(.+)""".r

  /** for synchornous people */
  def resolve(ref: FullSpecRef, e: Either[P, EMsg]): P = {
    // resolve EMrg's parameters in an empty context and run it and await?
    e.fold(
      p => p,
      m => DomEngineUtils.runMsgSync(new DieselMsg(m, DieselTarget.ENV("n/a")))
          .getOrElse(P.undefined(Diesel.PAYLOAD))
    )
  }

  // resolve for messages that return an option not a list - reuse the list one
  def resolve(realm: String, ref: FullSpecRef, e: Either[Option[DieselAsset[_]], EMsg]): Option[DieselAsset[_]] = {
    resolve(realm,
      ref,
      e.fold(
        p => Left(DIQueryResult(p.toList.size, p.toList)),
        m => Right(m)
      )
    ).data.headOption
  }

  /** O to DieselAsset */
  def oToA(o: O, ref: FullSpecRef, j: collection.Map[String, Any], realm: String) = {
    var r = j.get("assetRef")
        .filter(_.isInstanceOf[collection.Map[String, _]])
        .map(_.asInstanceOf[collection.Map[String, _]])
        .map(_.toMap)
        .map(SpecRef.fromJson)
    val key = r.map(_.key).orElse(j.get("key")).mkString

    if (r.exists(_.cls.isEmpty)) {
      r = r.map(_.copy(cls = o.base))
    }

    val dom = WikiDomain(realm)
    val inv = dom.rdom.classes.get(o.base).flatMap(c => dom.findPluginsForClass(c).headOption)

    new DieselAsset[O](SpecRef.make(realm, "", "", o.base, o.name), o)
    new DieselAsset[O](
      ref = r.getOrElse(new FullSpecRef(
        inv.map(_.name).getOrElse("n/a"),
        inv.map(_.conn).getOrElse("n/a"),
        o.base,
        key,
        "",
        ""
      )
      ),
//      DomInventories.oFromJ(name, j, c, classOname(c), filterAttrs)
      value = o,
      valueO = Some(o)
    )
  }

  /** json map to DieselAsset */
  def jToA(p: P, j: collection.Map[String, Any], realm: String) = {
    var r = j.get("assetRef")
        .filter(_.isInstanceOf[collection.Map[String, _]])
        .map(_.asInstanceOf[collection.Map[String, _]])
        .map(_.toMap)
        .map(SpecRef.fromJson)
    val key = r.map(_.key).orElse(j.get("key")).mkString

    if (r.exists(_.cls.isEmpty)) {
      r = r.map(_.copy(cls = p.ttype.schema))
    }

    val dom = WikiDomain(realm)
    val inv = dom.rdom.classes.get(p.ttype.getClassName).flatMap(c => dom.findPluginsForClass(c).headOption)

    new DieselAsset[P](
      ref = r.getOrElse(new FullSpecRef(
        inv.map(_.name).getOrElse("n/a"),
        inv.map(_.conn).getOrElse("n/a"),
        p.ttype.getClassName,
        key,
        "",
        ""
      )
      ),
//      DomInventories.oFromJ(name, j, c, classOname(c), filterAttrs)
      value = p,
      valueO = Some(
        O(
          key,
          p.ttype.schema,
          flattenJson(p)(ECtx.empty)
        )
      )
    )
  }

  /** for synchornous people */
  def resolve(realm: String, ref: FullSpecRef, e: Either[DIQueryResult, EMsg]): DIQueryResult = {
    // resolve EMrg's parameters in an empty context and run it and await?
    e.fold(
      p => p,
      m => {
        val p = DomEngineUtils.runMsgSync(new DieselMsg(m, DieselTarget.ENV(realm)))

        if (p.isEmpty || !p.get.isOfType(WTypes.wt.JSON) && !p.get.isOfType(WTypes.wt.ARRAY)) {
          log("sub-flow return nothing or not a list - so no asset found!")
          DIQueryResult(0)
        } else if (p.get.isOfType(WTypes.wt.JSON)) {
          val j = p.get.calculatedTypedValue(ECtx.empty).asJson
          val c = WikiDomain(realm).rdom.classes.get(p.get.ttype.schema)

          if (c.isDefined) {
            val k = j.toMap.get("key").getOrElse(j.toMap.get("ref").mkString).toString
            val o = oFromJMap(k, j.toMap, c.get, c.get.name, Array.empty)
            DIQueryResult(1, List(oToA(o, ref, j.toMap, realm)))
          }
          else
            DIQueryResult(1, List(jToA(p.get, j, realm)))
        } else {
          // array
          val l = p.get.calculatedTypedValue(ECtx.empty).asArray
          val c = WikiDomain(realm).rdom.classes.get(p.get.ttype.schema)

          DIQueryResult(l.size, l.collect {
            case o: P => {
              val j = o.calculatedTypedValue(ECtx.empty).asJson
              val c = WikiDomain(realm).rdom.classes.get(p.get.ttype.schema)

              if (c.isDefined) {
                val k = j.toMap.get("key").getOrElse(j.toMap.get("ref").mkString).toString
                val o = oFromJMap(k, j.toMap, c.get, c.get.name, Array.empty)
                oToA(o, ref, j.toMap, realm)
              }
              else
                jToA(o, j, realm)
            }
            case j: collection.Map[String, Any] => {
              val c = {
                WikiDomain(realm).rdom.classes.get(p.get.ttype.schema)
                    .orElse(
                      WikiDomain(realm).rdom.classes.get(ref.cls) // assume it's of the type we were looking for
                    )
              }

              if (c.isDefined) {
                val k = j.toMap.get("key").getOrElse(j.toMap.get("ref").mkString).toString
                val o = oFromJMap(k, j.toMap, c.get, c.get.name,
                  Array.empty)
                oToA(o, ref, j.toMap, realm)
              }
              else
                jToA(P.fromSmartTypedValue(Diesel.PAYLOAD, j), j, realm)
            }
            case x@_ => throw new DieselExprException("Unknown type for: " + x)
          }.toList)
        }
      }
    )
  }

  def doAction(dom: RDomain, conn: String, action: String, completeUri: String, epath: String): String = {
    try {
      action match {
//      case "testConnection" => DomInventories.resolve(testConnection(dom, epath)).currentStringValue
//      case "findByRef" => findByRefs(dom, epath)
//      case "findByQuery" => findByQuerys(dom, epath)
//      case "listAll" => xlistAll(dom, epath)
//
//      case "accessToken" => accessToken
//      case "attrs" => getEntityAttrs(dom, action, epath)
//      case "sample" => redirectToSample(dom, action, epath)
//      case "listClasses" => listClasses(dom, epath)
//      case "metaClass" => metaClass(dom, epath)
//      case "metaAttrs" => metaAttrs(dom, epath)
//      case "makeClass" => makeClass(dom, epath, loadClasses(dom))
//      case "makeAllClasses" => makeAllClasses(dom, action, epath)
        case _ => throw new NotImplementedError(s"doAction $action - $completeUri - $epath")
      }
    } catch {
      case e: Throwable =>
//      resetOnError(e)
        throw e
    }
  }

}

