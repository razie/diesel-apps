/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.dom

import com.razie.pub.comms.{CommRtException, Comms}
import java.net.{HttpURLConnection, URI}
import org.json.JSONObject
import razie.Snakk._
import razie.diesel.Diesel
import razie.diesel.dom.RDOM._
import razie.diesel.engine.nodes.EMsg
import razie.diesel.expr.ECtx
import razie.tconf.{DSpecInventory, FullSpecRef, SpecRef}
import razie.wiki.Sec
import razie.{Snakk, clog, js}
import scala.collection.mutable
import scala.util.Try

/** ODATA CRM domain plugin */
class DomInvOdataCRMPlugin(
  override val name: String = "d365odata",
  var props: Map[String, String] = Map.empty
) extends DomInventory {

  // annotation specific to these classes
  final val ODATA_NAME = "odata.name"


  override def conn = props.getOrElse("conn.name", "default")

  var realm: String = ""
  var env: String = ""
  var specInv: Option[DSpecInventory] = None
  var iprops: Map[String, String] = Map.empty

  /**
    * can this support the class? You can look at it's annotations etc
    */
  override def isDefinedFor(realm: String, c: C): Boolean = {
    c.props.find(_.name == ODATA_NAME).isDefined || super.isDefinedFor(realm, c)
  }

  /** if the ReactorMod is present and a connection, create it - just one conn */
  override def mkInstance(irealm: String, ienv: String, wi: DSpecInventory, newName: String, dprops: Map[String,
      String] = Map.empty)
  : List[DomInventory] = {
    this.iprops = dprops
    realm = irealm
    env = ienv
    specInv = Some(wi)
    reset(iprops, newName)
  }

  private def reset(iprops: Map[String, String], newName: String): List[DomInventory] = {
    val pspec = SpecRef("", realm + ".ReactorMod:ODataCrmDomainPlugin", realm)
    val props = SpecRef("", realm + ".Form:ODataCrmDomainProps", realm)

    specInv
        .flatMap(_.findSpec(pspec))
        .flatMap(x => specInv.flatMap(_.findSpec(props)))
        .toList
        .map { wprops =>
          val ret = new DomInvOdataCRMPlugin(newName, iprops ++ wprops.allProps)
          ret.realm = realm
          ret.specInv = this.specInv
          ret.iprops = this.iprops
          ret.env = this.env
          ret
        }

    // todo add observer when form changes and not reset all the time
    // todo design mechanism for wiki changes propagating and objects changing state
  }

  def configuredAccessToken = props.getOrElse("odata.accessToken", "")

  def authUrl = props.getOrElse("odata.authUrl", "")

  def authClient = props.getOrElse("odata.authClient", "")

  def authSecret = props.getOrElse("odata.authSecret", "")

  def URL = props.getOrElse("odata.url", "")


  def accessToken = iAccessToken.getOrElse {
    if (configuredAccessToken.length > 10) {
      iAccessToken = Some(configuredAccessToken)
    } else {
      val j = Snakk.json(
        url(
          authUrl,
          Map("Content-type" -> "application/x-www-form-urlencoded")
        ).form(
          Map(
            "grant_type" -> "client_credentials",
            "client_id" -> authClient,
            "client_secret" -> (authSecret),
            "resource" -> URL
          ) ++ {
            // todo ugliest hack - when using backend, put the sud/iss sequence as authClient
            if (authClient.contains("iss=Omniware"))
              authClient.split("&").filter(_.contains("=")).map { a =>
                val x = a.split("=")
                (x(0), Sec.decUrl(x(1)))
              }.toList.toMap
            else
              Map.empty[String, String]
          }
        )
      )

      clog << "oauth reply: " + j
      iAccessToken = Some(j \@ "access_token")
    }
    iAccessToken.get
  }

  var iAccessToken: Option[String] = None

  def classOname(c: C): String =
    c.props.find(_.name == ODATA_NAME).map(_.calculatedValue(ECtx.empty)).getOrElse(c.name)

  // reset during calls
  var completeUri: String = ""

  /** html for the supported actions */
  def htmlActions(elem: DE): String = {
    elem match {
      case c: C => {
        val oname = classOname(c)

        def mkMetA = s"""<a href="/diesel/plugin/$name/$conn/attrs/${c.name}">attrs</a>"""
        def mkMet = s"""<a href="$URL/EntityDefinitions(LogicalName='$oname')">def</a>"""
        def mkSample = s"""<a href="$URL/${oname}s?$$top=1">sample</a>"""
        def mkListAll = s"""<a href="/diesel/plugin/$name/$conn/listAll/${c.name}">listAll</a>"""

        s"$mkMet | $mkMetA | $mkSample | $mkListAll"
      }

      case _ => "?"
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
  def doAction(dom: RDomain, conn:String, action: String, completeUri: String, epath: String): String = {
    try {
      // todo for now to force reloading the attributes
      reset(iprops, name)

      this.completeUri = completeUri

      action match {
        case "testConnection" => DomInventories.resolve(testConnection(dom, epath)).currentStringValue
        case "findByRef" => findByRefs(dom, epath)
        case "findByQuery" => findByQuerys(dom, epath)
        case "listAll" => xlistAll(dom, epath)

        case "accessToken" => accessToken
        case "attrs" => getEntityAttrs(dom, action, epath)
        case "sample" => redirectToSample(dom, action, epath)
        case "listClasses" => listClasses(dom, epath)
        case "metaClass" => metaClass(dom, epath)
        case "metaAttrs" => metaAttrs(dom, epath)
        case "makeClass" => makeClass(dom, epath, loadClasses(dom))
        case "makeAllClasses" => makeAllClasses(dom, action, epath)
        case _ => throw new NotImplementedError(s"doAction $action - $completeUri - $epath")
      }
    } catch {
      case e: Throwable =>
        resetOnError(e)
        throw e
    }
  }

  /** reset oauth tokens etc on error */
  override def resetOnError(error: Throwable) = {
    // protection against stale tokens
    this.iAccessToken = None
  }

  /** html for the supported actions */
  override def testConnection(dom: RDomain, epath: String): Either[P, EMsg] = {
    val s =
      s"""testing auth...\n
         |
         |authUrl=$authUrl\n
         |authClient=$authClient\n
         |authSecret=$authSecret\n
         |\n
         |accessToken=${accessToken}\n
         |\n
         |listClasses=${listClasses(dom, epath).split(",").size}\n
         |
     """.stripMargin
    Left(P(Diesel.PAYLOAD, s))
  }

  override def connect(dom: RDomain, env: String): Either[P, EMsg] =
    Left(P(Diesel.PAYLOAD, "ok"))

  /** html for the supported actions */
  private def getEntityAttrs(dom: RDomain, action: String, epath: String): String = {
    dom.classes.get(epath) match {
      case Some(c) => {
        val oname = classOname(c)

        getEntityAttrsFor(dom, oname, loadClasses(dom))
      }

      case _ => "eh?"
    }
  }

  val filterAttrs =("owningteam,modifiedby,owninguser,owneridname,createdonbehalfby,utcconversiontimezonecode,"+
    "importsequencenumber,createdbyyominame,owningbusinessunit,modifiedbyname,"+
    "owningteam,modifiedby,modifiedbyyominame,createdby,timezoneruleversionnumber,owneridtype,owneridyominame,"+
    "modifiedon,modifiedonbehalfbyyominame,createdbyname,createdon,createdonbehalfbyname,modifiedonbehalfbyname,"+
    "versionnumber,modifiedonbehalfby,ownerid,overriddencreatedon,createdonbehalfbyyominame").split(",")

  /** html for the supported actions */
  private def getEntityAttrsFor (dom: RDomain, epath: String, classNames:Map[String,String], collectRefs : Option[mutable.HashMap[String,String]] = None): String = {
    val host = new URI(completeUri).getHost

    def mkMetA = URL + s"/api/data/v8.2/EntityDefinitions(LogicalName='${epath}')/Attributes"

    val b = Snakk.body(Snakk.url(
      mkMetA,
      Map("Authorization" -> accessToken)
    ))

        val jj = Snakk.jsonParsed(b)

        val m = js.fromObject(jj)

        val buf =
          m("value")
            .asInstanceOf[List[Map[String, Any]]]
            .filter(_.get("AttributeType").exists(_ != "Virtual"))
            .filter(x=> !filterAttrs.contains(x.get("LogicalName").mkString))
            .map { a =>

              val n = a.get("LogicalName").mkString

              var s = s"  $n"

              if (a.get("AttributeType").exists(_ == "Lookup")) {
                val t = a("Targets").asInstanceOf[List[Any]].head.toString

                val d = a("DisplayName")
                  .asInstanceOf[Map[String, Any]]("UserLocalizedLabel")
                  .asInstanceOf[Map[String, Any]]("Label")
                  .toString.replaceAll("[ ()]", "")

                val cn = classNames.getOrElse(t, t)
                s = s"  $n : <> $cn"

                collectRefs.map (_.put(t.toString, cn)) // collect all the refs
              }
              s
            }.mkString(",\n")

        buf
  }

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

  /** query for the first sample of this class */
  private def redirectToSample(dom: RDomain, action: String, epath: String): String = {
    dom.classes.get(epath) match {
      case Some(c: C) => {
        val oname = classOname(c)

        def mkSample = s"""<a href="$URL/${oname}s?$$top=1">sample</a>"""

        val j = Snakk.jsonParsed(mkSample)

        j.toString(2)
      }

      case _ => "?"
    }
  }

  /** list classes */
  private def loadClasses(dom: RDomain): Map[String,String] = {
    val host = new URI(completeUri).getHost

    val snak = crmJson(URL+s"/api/data/v8.2/EntityDefinitions?$$select=LogicalName,DisplayName")

    val v = snak \ "value"

    val buf = v.nodes.map(snak.wrap).map {v=>

      val n = v \@ "LogicalName"
      val ln = (v \ "DisplayName" \ "UserLocalizedLabel" \@@ "Label" OR "").toString.replaceAllLiterally(" ", "").replaceAllLiterally("/", "")
      (n,ln)
    }

    buf.toSeq.toMap
  }

  /** list classes */
  private def listClasses(dom: RDomain, epath: String): String = {
    val host = new URI(completeUri).getHost

    val classNames = loadClasses(dom)

    val buf =
      classNames.keySet
//        .filter(_.get("LogicalName").exists(_.toString startsWith "omni_"))
        .toList
        .sorted
        .map{t=>
          val d = classNames(t)
          s"""$t - $d |
             | <a href="/diesel/plugin/$name/$conn/metaClass/$t">met</a> |
             | <a href="/diesel/plugin/$name/$conn/attrs/$d">attrs</a> |
             | <a href="/diesel/plugin/$name/$conn/makeClass/$t">make</a>
           """.stripMargin

        }
        .mkString("<br>\n")

    "<br>" + buf
  }

  /**
    * find by entity/ref
    */
  private def findByRefs(dom:RDomain, epath: String, collectRefs:Option[mutable.HashMap[String,String]]=None): String = {
    val host = new URI(completeUri).getHost
    val PAT = """([^/]+)/(.+)""".r
    val PAT(cls, id) = epath

    dom.classes.get(cls).map { classDef=>
      val oname = classOname(classDef)
      val u = URL+s"/api/data/v8.2/${oname}s($id)"

      val b = crmJson(u)

      b.node.j.asInstanceOf[JSONObject].toString(2)
    } getOrElse
    "Class not found..."
  }

  /**
    * find by field value
    */
  private def findByQuerys(dom:RDomain, epath: String, collectRefs:Option[mutable.HashMap[String,String]]=None): String = {
    val host = new URI(completeUri).getHost
    val PAT = """([^/]+)/(.+)/(.+)""".r
    val PAT(cls, field, id) = epath
    val filter = Sec.encUrl(s"$field eq $id")

    doQuery(cls, dom, filter, collectRefs)
  }

  /**
    * list all
    */
  private def xlistAll(dom: RDomain, epath: String, collectRefs: Option[mutable.HashMap[String, String]] = None)
  : String = {
//    : Either[List[DieselAsset[_]], EMsg] = {
    doQuery(epath, dom, "", collectRefs)
  }

  /**
    * list all
    */
  private def doQuery (cls:String, dom:RDomain, filter: String, collectRefs:Option[mutable.HashMap[String,String]]=None): String = {
    val host = new URI(completeUri).getHost

    dom.classes.get(cls).map { classDef=>
      val oname = classOname(classDef)
      val u =
        if(filter.length > 0) URL + s"/api/data/v8.2/${oname}s?$$filter=" + filter
        else URL + s"/api/data/v8.2/${oname}s?$$top=100"

      val b1 = crmJson(u)

      val b = crmJson(u)

      return b1.node.j.asInstanceOf[JSONObject].toString(2)

      val v = b \ "value"

      v.nodes.headOption.map {n=>
        val jo = n.j.asInstanceOf[JSONObject]

        val o = oFromJ("x", jo, classDef)

        n.j.asInstanceOf[JSONObject].toString(2)
      } getOrElse
      "entity not found..."
    } getOrElse
      "Class not found..."
  }

  /** turn a json value into a nice object, merge with class def and mark refs etc */
  def oFromJ(key: String, j: JSONObject, c: C) = {
    // see DomInventories.oFromJ - but this one needs more filtering

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
          val oname = classOname(c)

          c.parms.find(_.name == kn).map { cp =>
            cp.copy(dflt = value.toString) // todo add PValue
          } getOrElse {
            // key refs
            if (kn.startsWith("_") && kn.endsWith(("_value"))) {
              val PAT = """_(.+)_value""".r
              val PAT(n) = kn

              c.parms.find(_.name == n).map { cpk =>
                cpk.copy(dflt = value.toString) // todo add PValue
              } getOrElse {
                P(kn, value)
              }
            } else
              P(kn, value)
          }
        }

    O(key, c.name, parms)
  }

  def crmJson(url:String) = {
    clog << "Snakking: " + url
    try {
      val b = Snakk.body(Snakk.url(
        url,
        Map("Authorization" -> accessToken)
      ))

      Snakk.json(b)
    } catch {
      case cre:CommRtException if cre.uc != null => {
        val resCode = cre.uc.getHeaderField(0)
        val c = Comms.readStream(cre.uc.asInstanceOf[HttpURLConnection].getErrorStream)
        val msg = "Could not fetch data from url " + url + ", resCode=" + resCode + ", content=" + c
        clog << "SNAKK COMM ERR: " + (msg)

        val jc = if(c.startsWith("{") && c.endsWith("}")) c else s""""$c""""

        val jo = new JSONObject()
        jo.append("url", url)

        if(c.startsWith("{") && c.endsWith("}"))
          jo.append("error", c)
        else
        jo.append("error", c)

        // just in case it may have expired - reset the token
        iAccessToken=None

        Snakk.json(jo)
      }
    }
  }

  /**
    * find by entity/ref
    */
  override def findByRef(dom: RDomain, ref: FullSpecRef, collectRefs: Option[mutable.HashMap[String, String]] = None)
  : Either[Option[DieselAsset[_]], EMsg] = {
    val host = new URI(completeUri).getHost

    Left(dom.classes.get(ref.cls).map { classDef =>
      val oname = classOname(classDef)

      // the idiots use an englishly-correct plural
      val plural = if (oname endsWith "y") oname.take(oname.length - 1) + "ies" else oname + "s"

      val u = URL + s"/api/data/v8.2/$plural(${ref.key})"

      val b = crmJson(u)

//      b.node.j.asInstanceOf[JSONObject].toString(2)

      val o = oFromJ(ref.key, b.node.j.asInstanceOf[JSONObject], classDef)
      new DieselAsset[O](ref, o)
    })
  }

  /**
    * find by field value
    *
    * if the field and id is null, then no filter
    */
  override def findByQuery(dom: RDomain, ref: FullSpecRef, epath: String, collectRefs: Option[mutable.HashMap[String,
      String]] = None): Either[List[DieselAsset[_]], EMsg] = {
    val host = new URI(completeUri).getHost
    val PAT = DomInventories.CLS_FIELD_VALUE
    val PAT(cls, field, id) = epath

    Left(
      dom.classes.get(cls).toList.flatMap { classDef =>
        val oname = classOname(classDef)

        // the idiots use an englishly-correct plural
        val plural = if (oname endsWith "y") oname.take(oname.length - 1) + "ies" else oname + "s"

        val filter = {
          if (id == "" || id == "*" || id == "'*'")
            s"$$top=50"
          else
            s"$$filter=" + Sec.encUrl(s"$field eq $id")
        }

        val u = URL + s"/api/data/v8.2/$plural?" + filter

        val b = crmJson(u)

        val v = b \ "value"

        v.nodes.toList.map { n =>
          val jo = n.j.asInstanceOf[JSONObject]
          val key = if (jo.has(oname + "id")) jo.get(oname + "id").toString else epath
          val o = oFromJ(key, jo, classDef)
          new DieselAsset[O](SpecRef.make(ref.realm, name, conn, classDef.name, key), o)
        }
    }
    )
  }

  /**
    * compose a clas defn
    * @param epath CRM name like omni_invoice
    */
  private def metaClass(dom:RDomain, epath: String): String = {
    val host = new URI(completeUri).getHost

    val snak = crmJson(URL+s"/api/data/v8.2/EntityDefinitions?$$filter=" + Sec.encUrl(s"LogicalName eq '${epath}'"))

    snak.node.j.asInstanceOf[JSONObject].toString(2)
  }

  /**
    * compose a clas defn
    * @param epath CRM name like omni_invoice
    */
  private def metaAttrs(dom:RDomain, epath: String): String = {
    val host = new URI(completeUri).getHost

    def mkMetA = URL + s"/api/data/v8.2/EntityDefinitions(LogicalName='${epath}')/Attributes"
    val snak = crmJson(mkMetA)

    snak.node.j.asInstanceOf[JSONObject].toString(2)
  }

  /**
    * compose a clas defn
    * @param epath CRM name like omni_invoice
    */
  private def makeClass(dom:RDomain, epath: String, classNames:Map[String,String], collectRefs:Option[mutable.HashMap[String,String]]=None): String = {
    val host = new URI(completeUri).getHost

    val snak = crmJson(URL+s"/api/data/v8.2/EntityDefinitions?$$filter=" + Sec.encUrl(s"LogicalName eq '${epath}'"))

    val v = snak.wrap(snak \\\ "value")

    val n = v \@ "LogicalName"
    val ln = (v \ "DisplayName" \ "UserLocalizedLabel" \@@ "Label" OR "").toString.replaceAllLiterally(" ", "")

    var s = ""
    s += s"""$$anno (odata.name="$n")\n"""
    s += s"$$class $ln (\n"
    s += getEntityAttrsFor(dom,n, classNames, collectRefs)
    s += s"\n)\n"

    s
  }

  /** compose a clas defn */
  private def makeAllClasses(dom: RDomain, action: String, epath: String): String = {
    val classNames = loadClasses(dom)
    val refs = new mutable.HashMap[String,String]()

    val c = classNames.keys.toSeq.sorted.filter(_.startsWith("omni")).map {cn=>
      (cn, s"## ${classNames(cn)} ($cn)\n\n" + makeClass(dom, cn, classNames, Some(refs)))
    }.toMap

    // all refs, no duplicates and no name conflicts (i.e. product is Product would overwrite omni_product which is also Product
    val c2 = refs.keys
      .toSeq
      .sorted
      .filterNot{n=>
        val dn = refs(n)
        c.contains(n) || classNames.exists(e=> e._2==dn && c.contains(e._1))
      }.map {cn=>
      (cn, s"## ${classNames(cn)} ($cn)\n\n" + Try {
        makeClass(dom, cn, classNames, Some(refs))
      }.getOrElse("Error...")
      )
    }.toMap

    val call = c ++ c2

   val cs = call.keys.toSeq.sorted.map {k=>
     call.get(k).mkString
   }.mkString("\n\n")

    var s = ""
    s += s"""Domain... total ${call.size} classes \n\n"""
    s += cs
    s += s"\n\n"

    s
  }

}

