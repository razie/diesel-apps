/**
  * ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.dom

import razie.diesel.dom.RDOM.P.asString
import razie.diesel.dom.RDOM._
import razie.diesel.engine.nodes.EMsg
import razie.diesel.expr.ECtx
import razie.tconf.{DSpecInventory, FullSpecRef}
import razie.{Snakk, js}
import scala.collection.mutable

/** based on diesel rules domain plugin */
class DieselRulesInventory(
  override val name: String = "diesel",
  var props: Map[String, String] = Map.empty
) extends DomInventory {

  var env: String = ""

  override def conn = props.getOrElse("conn.name", "default")

  var realm: String = ""
  var specInv: Option[DSpecInventory] = None
  var iprops : Map[String,String] = Map.empty

  /** if the ReactorMod is present and a connection, create it - just one conn */
  override def mkInstance(irealm: String, ienv:String, wi: DSpecInventory, newName:String, dprops: Map[String, String] = Map.empty): List[DomInventory] = {
    // todo optimize - load only if a set of classes matches or something
    // if classes are annotated with db.name or something
    val x = wi.querySpecs(irealm, "", "", "spec")

    wi.querySpecs(irealm, "", "", "spec")
        .find { spec =>
            // don't have to actually parse, eh?
//          val rest = spec.collector.getOrElse(RDomain.DOM_LIST, List[Any]()).asInstanceOf[List[Any]]
//          val rules = rest.find(_.isInstanceOf[ERule]).map(_.asInstanceOf[ERule])
//          rules.exists(_.e.ea == "diesel.inv.impl.findByRef")
          spec.content.contains("diesel.inv.impl.findByRef")
        }.map { wprops =>
      val ret = new DieselRulesInventory(newName, iprops)// ++ wprops.allProps)
      ret.realm = irealm
      ret.specInv = Some(wi)
      ret.iprops = dprops
      ret.env = ienv
      ret
      // todo find EnvironmentProperties for env ?
    }.toList

    // todo add observer when form changes and not reset all the time
    // todo design mechanism for wiki changes propagating and objects changing state
  }

  final val TABLE = "table"

  override def testConnection(dom: RDomain, epath: String): Either[P,EMsg] = {
    Right(
      new EMsg(
      "diesel.inv.impl",
      "testConnection",
      List(
        P("inventory", this.name),
        P("connection", this.conn),
        P("env", this.env)
      )
      )
    )
  }

  override def connect(dom:RDomain, env:String): Either[P,EMsg] = {
    Right(
      new EMsg(
      "diesel.inv.impl",
      "connect",
      List(
        P("inventory", this.name),
        P("connection", this.conn),
        P("env", env)
      )
    )
    )
  }

  override def upsert(dom: RDomain, ref: FullSpecRef, asset:DieselAsset[_]) : Either[Option[DieselAsset[_]], EMsg] = {
    Right(
      new EMsg(
      "diesel.inv.impl",
        "upsert",
      List(
        P("inventory", this.name),
        P("connection", this.conn),
        P("className", ref.cls),
        P("key", ref.key),
        P.fromSmartTypedValue("assetRef", ref.toJson),
        asset.getValueP.copy(name = "entity")
      ))
    )
  }

  /** list all elements of class */
  override def listAll(dom: RDomain, ref: FullSpecRef, start: Long, limit: Long, collectRefs: Option[mutable
  .HashMap[String, String]] = None)
  : Either[List[DieselAsset[_]], EMsg] = {
    Right(
      new EMsg(
        "diesel.inv.impl",
        "listAll",
        List(
          P("inventory", this.name),
          P("connection", this.conn),
          P("className", ref.cls),
          P.fromSmartTypedValue("ref", ref.toJson)
        ))
    )
  }

  /**
    * remove by entity/ref
    */
  override def remove(dom: RDomain, ref: FullSpecRef)
  : Either[Option[DieselAsset[_]], EMsg] = {
    Right(
      new EMsg(
        "diesel.inv.impl",
        "remove",
        List(
          P("inventory", this.name),
          P("connection", this.conn),
          P("className", ref.cls),
          P.fromSmartTypedValue("ref", ref.toJson)
        ))
    )
  }

  def classOname(c: C): String =
    c.props.find(_.name == TABLE).map(_.calculatedValue(ECtx.empty)).getOrElse(c.name)

  // reset during calls
  var completeUri: String = ""

  /** html for the supported actions */
  def htmlActions(elem: DE): String = {
    elem match {
      case c: C => {
        val oname = classOname(c)

        def mkListAll2 = s"""<a href="/diesel/list2/${c.name}">listAll</a>"""

//        def mkListAll = s"""<a href="/diesel/dom/$name/$conn/${c.name}/listAll"><small>list(deprecated)</small></a>"""

        s"$mkListAll2 "
      }

      case _ => "n/a"
    }
  }

  /**
    * do an action on some domain entity (explore, browse etc)
    *
    * @param dom         the domain
    * @param action      the action to execute
    * @param completeUri the entire URL called (use it to get host/port etc)
    * @param epath       id of the entity
    * @return
    */
  def doAction(dom: RDomain, conn: String, action: String, completeUri: String, epath: String): String = {
    try {
      this.completeUri = completeUri
      val ref = new FullSpecRef(this.name, conn, epath, "", "", realm)

      action match {
        case "testConnection" => {
          DomInventories.resolve(ref, testConnection(dom, epath)).currentStringValue
        }
        case "listAll" => {
          DomInventories.resolve(
            dom.name,
            ref,
            listAll(dom, ref, 0, 100)
          ).map { da =>
            asString(da.getValueP)
          }.mkString("\n")
        }
        case _ => throw new NotImplementedError(s"doAction $action - $completeUri - $epath")
      }
    } catch {
      case e: Throwable =>
        // something to reset/clean/reconnect?
        // .todo send reconnect
        throw e
    }
  }

  val filterAttrs = "".split(",")

  private def getValueFromResp(url: String): List[Map[String, Any]] = {
    val jj = Snakk.jsonParsed(Snakk.body(Snakk.url(url)))
    val m = js.fromObject(jj)

    m("value")
        .asInstanceOf[List[Map[String, Any]]]
  }

  private def getValueFromBody(b: String): List[Map[String, Any]] = {
    val jj = Snakk.jsonParsed(b)
    val m = js.fromObject(jj)

    m("value")
        .asInstanceOf[List[Map[String, Any]]]
  }

  private def mprops: Map[String, Any] = List(
    P.fromTypedValue("inventory", name),
    P.fromTypedValue("connection", conn)
  ).map(x => (x.name, x)).toMap


  /** list all elements of class */
  def pToList(ref: FullSpecRef, p: Option[P]): List[DieselAsset[_]] = {
    p
        .filter(_.isOfType(WTypes.wt.ARRAY))
        .toList
        .flatMap(_.value.map(_.asArray))
        .map(x =>
          DieselAsset(
            ref.copy(key = "n/a"),
            P.toObject(x)
          )
        )
    Nil
  }

  /**
    * find by field value
    *
    * if the field and id is null, then no filter
    */
  override def findByQuery(dom: RDomain, ref: FullSpecRef, epath: Either[String, collection.Map[String, Any]],
                           from: Long = 0, size: Long = 100,
                           collectRefs: Option[mutable.HashMap[String, String]] = None):
  Either[List[DieselAsset[_]], EMsg] = {

    val attrs = epath.fold(
      s => {
        // query by path
        val PAT = DomInventories.CLS_FIELD_VALUE
        val PAT(cls, field, id) = epath.left.get

        {
          if (id == "" || id == "*" || id == "'*'")
            P.fromSmartTypedValue("query", Map())
          else
            P.fromSmartTypedValue("query", Map(field -> id))
        }
      },
      m => P.fromSmartTypedValue("query", m)
    )

    Right(
      new EMsg(
        "diesel.inv.impl",
        "findByQuery",
        List(
          P("inventory", this.name),
          P("connection", this.conn),
          P("className", ref.cls),
          attrs
        ))
    )
  }

  /**
    * find by entity/ref
    */
  override def findByRef(dom: RDomain, ref: FullSpecRef, collectRefs: Option[mutable.HashMap[String, String]] = None)
  : Either[Option[DieselAsset[_]], EMsg] = {

    Right(
      new EMsg(
        "diesel.inv.impl",
        "findByRef",
        List(
          P("inventory", this.name),
          P("connection", this.conn),
          P("className", ref.cls),
          P.fromSmartTypedValue("ref", ref.toJson)
        ))
    )
  }

}

