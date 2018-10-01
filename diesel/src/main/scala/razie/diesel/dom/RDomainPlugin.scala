package razie.diesel.dom

import java.net.URI

import razie.{Snakk, js}
import razie.diesel.dom.RDOM._
import razie.Snakk._
import razie.js.jt

import scala.collection.mutable
import scala.util.Try

/**
  * a domain plugin - can adapt a domain to an external implementation
  */
trait RDomainPlugin {
  def name: String

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
}

/** some helpers */
object RDomainPlugins {
  val plugins = new CRMRDomainPlugin :: new DefaultRDomainPlugin :: Nil

  def htmlActions(c: DE) = {
    RDomainPlugins.plugins.foldLeft("")((a, b) => a + (if (a != "") " <b>|</b> " else "") + b.htmlActions(c))
  }
}

class DefaultRDomainPlugin extends RDomainPlugin {
  override def name = "default"

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

class CRMRDomainPlugin extends RDomainPlugin {
  override def name = "d365odata"

  final val ODATA_NAME = "odata.name"

  def URL = "https://omniitelyatest.crm3.dynamics.com/api/data/v8.2"

  // reset during calls
  var completeUri: String = ""

  /** html for the supported actions */
  def htmlActions(elem: DE): String = {
    elem match {
      case c: C => {
        val oname = c.props.find(_.name == ODATA_NAME).map(_.calculatedValue(ECtx.empty)).getOrElse(c.name)

        //        def mkMetA = s"""<a href="$URL/EntityDefinitions(LogicalName='$oname')/Attributes?$$select=LogicalName">attrs</a>"""
        //        def mkMet = s"""<a href="$URL/EntityDefinitions(LogicalName='$oname')">def</a>"""
        //        def mkSample = s"""<a href="$URL/${oname}s?$$top=1">sample</a>"""

        def mkMetA = s"""<a href="/diesel/plugin/$name/attrs/${c.name}">attrs</a>"""

        def mkMet = s"""<a href="$URL/EntityDefinitions(LogicalName='$oname')">def</a>"""

        def mkSample = s"""<a href="$URL/${oname}s?$$top=1">sample</a>"""

        s"$mkMet | $mkMetA | $mkSample"
      }

      case _ => "?"
    }
  }

  /**
    * do an action on some domain entity (explore, browse etc)
    *
    * @param r           the domain
    * @param action      the action to execute
    * @param completeUri the entire URL called (use it to get host/port etc)
    * @param epath       id of the entity
    * @return
    */
  def doAction(dom: RDomain, action: String, completeUri: String, epath: String): String = {
    this.completeUri = completeUri

    action match {
      case "attrs" => getEntityAttrs(dom, action, epath)
      case "sample" => redirectToSample(dom, action, epath)
      case "listClasses" => listClasses(dom, action, epath)
      case "aclass" => makeAClass(dom, action, epath)
      case "classes" => makeAllClasses(dom, action, epath)
      case _ => ???
    }
  }

  /** html for the supported actions */
  private def getEntityAttrs (dom: RDomain, action: String, epath: String): String = {
    dom.classes.get(epath) match {
      case Some(c) => {
        val oname = c.props.find(_.name == ODATA_NAME).map(_.calculatedValue(ECtx.empty)).getOrElse(c.name)

        getEntityAttrsFor(dom,action,oname)
      }

      case _ => "eh?"
    }
  }

  def msg (s:String) = {
    s"""$completeUri/diesel/react/$s&resultMode=value&env=itelyatest&X-Api-Key=omni123$$"""
  }

  val filterAttrs =("owningteam,modifiedby,owninguser,owneridname,createdonbehalfby,utcconversiontimezonecode,"+
    "importsequencenumber,createdbyyominame,owningbusinessunit,modifiedbyname,"+
    "owningteam,modifiedby,modifiedbyyominame,createdby,timezoneruleversionnumber,owneridtype,owneridyominame,"+
    "modifiedon,modifiedonbehalfbyyominame,createdbyname,createdon,createdonbehalfbyname,modifiedonbehalfbyname,"+
    "versionnumber,modifiedonbehalfby,ownerid,overriddencreatedon,createdonbehalfbyyominame").split(",")

  /** html for the supported actions */
  private def getEntityAttrsFor (dom: RDomain, action: String, epath: String, collectRefs : Option[mutable.HashMap[String,String]] = None): String = {
    val classes = loadClasses(dom,action,epath)

        val host = new URI(completeUri).getHost

        def mkMetA = msg(s"""api.crm/getMetaAttrs?name=$epath""")

//    return Snakk.body(Snakk.url(mkMetA))

        val jj = Snakk.jsonParsed(Snakk.body(Snakk.url(mkMetA)))
        val m = js.fromObject(jj)

        //          val mm = js.jt(m) {
        //              case ("/", "value", l: List[_]) => ("attrs" -> jt(l) {
        //                case (_, n, avalue) if Array(
        //                  "LogicalName", "IsPrimaryName", "IsPrimaryId", "AttributeType", "Targets"
        //                ).contains(n)=> (n, avalue)
        //                case (_, _, _) => ("", "")
        //              })
        //            }
        //          razie.js.tojsons(mm.toMap)

        val buf =
          m("value")
            .asInstanceOf[List[Map[String, Any]]]
            .filter(_.get("AttributeType").exists(_ != "Virtual"))
            .filter(x=> !filterAttrs.contains(x.get("LogicalName").mkString))
            .map { a =>

              val n = a.get("LogicalName").mkString

              var s = s"  $n"

              if (a.get("AttributeType").exists(_ == "Lookup")) {
                val t = a("Targets").asInstanceOf[List[Any]].head
                val d = a("DisplayName")
                  .asInstanceOf[Map[String, Any]]("UserLocalizedLabel")
                  .asInstanceOf[Map[String, Any]]("Label")
                  .toString.replaceAll("[ ()]", "")
                s = s"  $n : <> $d" // ($t)"

                collectRefs.map (_.put(t.toString, d)) // collect all the refs
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

  /** query for the first sample of this class */
  private def redirectToSample(dom: RDomain, action: String, epath: String): String = {
    dom.classes.get(epath) match {
      case Some(c: C) => {
        val oname = c.props.find(_.name == ODATA_NAME).map(_.calculatedValue(ECtx.empty)).getOrElse(c.name)

        def mkSample = s"""<a href="$URL/${oname}s?$$top=1">sample</a>"""

        val j = Snakk.jsonParsed(mkSample)

        j.toString(2)
      }

      case _ => "?"
    }
  }

  /** list classes */
  private def loadClasses(dom: RDomain, action: String, epath: String): Map[String,String] = {
    val host = new URI(completeUri).getHost

    def mkMetA = msg(s"""api.crm/getMetaClasses?x=a""")

    val snak = Snakk.json(Snakk.url(mkMetA))
    val v = snak \ "value"

    val buf = v.nodes.map(snak.wrap).map {v=>
      import razie.OR
      val n = v \@ "LogicalName"
      val ln = (v \ "DisplayName" \ "UserLocalizedLabel" \@@ "Label" OR "").toString.replaceAllLiterally(" ", "")
      (n,ln)
    }

    buf.toSeq.toMap
  }

  /** list classes */
  private def listClasses(dom: RDomain, action: String, epath: String): String = {
    val host = new URI(completeUri).getHost

    def mkMetA = msg(s"""api.crm/getMetaClasses?x=a""")

    val buf =
      getValueFromResp(mkMetA)
//        .filter(_.get("LogicalName").exists(_.toString startsWith "omni_"))
        .map { a =>
        val n = a.get("LogicalName").mkString
//        val ln = v \ "DisplayName" \ "LocalizedLabels" \@@ "Label"
        n
      }.sorted.mkString(",\n")

    buf
  }

  /**
    * compose a clas defn
    * @param epath CRM name like omni_invoice
    */
  private def makeAClass(dom:RDomain, action: String, epath: String, collectRefs:Option[mutable.HashMap[String,String]]=None): String = {
    val host = new URI(completeUri).getHost

    def mkMetA = msg(s"""api.crm/getMetaClass?name=$epath""")

    val snak = Snakk.json(Snakk.url(mkMetA))
    val v = snak.wrap(snak \\\ "value")

    val n = v \@ "LogicalName"
    val ln = (v \ "DisplayName" \ "UserLocalizedLabel" \@@ "Label" OR "").toString.replaceAllLiterally(" ", "")
//    val ln = (v \ "DisplayName" \ "UserLocalizedLabel" \@@ "Label").replaceAllLiterally(" ", "")

    var s = ""
    s += s"""$$anno (odata.name="$n")\n"""
    s += s"$$class $ln (\n"
    s += getEntityAttrsFor(dom,action,n, collectRefs)
    s += s"\n)\n"

    s
  }

  /** compose a clas defn */
  private def makeAllClasses(dom: RDomain, action: String, epath: String): String = {
    val classes = loadClasses(dom,action,epath)
    val refs = new mutable.HashMap[String,String]()

    val c = classes.keys.toSeq.sorted.filter(_.startsWith("omni")).map {cn=>
      (cn, s"## ${classes(cn)} ($cn)\n\n" + makeAClass(dom,action,cn, Some(refs)))
    }.toMap

    val c2 = refs.keys.toSeq.sorted.filterNot(classes.contains).map {cn=>
      (cn, s"## ${classes(cn)} ($cn)\n\n" + Try {
        makeAClass(dom,action,cn, Some(refs))
      }.getOrElse("Error...")
      )
    }.toMap

   val cs = (c.keys ++ c2.keys).toSeq.sorted.map {k=>
     c.get(k).orElse(c2.get(k)).mkString
   }.mkString("\n\n")

    var s = ""
    s += s"""Domain... total ${c.size} classes \n\n"""
    s += cs
    s += s"\n\n"

    s
  }

}

