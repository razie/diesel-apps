/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.model

import com.mongodb.casbah.Imports._
import com.novus.salat._
import org.joda.time.DateTime
import razie.Log
import razie.db.RazSalatContext._
import razie.db._
import razie.wiki.Services
import razie.wiki.parser.WAST
import razie.wiki.admin.Audit

import scala.collection.mutable.ListBuffer

/** a simple wiki-style entry: language (markdown, mediawiki wikidot etc) and the actual source
  *
  * There is an "owner" property - owner is supposed to have special privileges
  */
@RTable
case class WikiEntry(
  category: String,
  name: String,
  label: String,
  markup: String,
  content: String,
  by: ObjectId,
  tags: Seq[String] = Seq(),
  realm:String = Wikis.RK,
  ver: Int = 1,
  parent: Option[ObjectId] = None,
  props: Map[String, String] = Map.empty,
  crDtm: DateTime = DateTime.now,
  updDtm: DateTime = DateTime.now,
  _id: ObjectId = new ObjectId()) {

  import WikiEntry._

  /** is this just an alias?
    *
    * an alias is a topic that only contains the alias markup: [[alias:xxx]]
    */
  def alias: Option[WID] = {
    val wikip2 = """\[\[alias:([^\]]*)\]\]"""
    val wikip2r = wikip2.r
    if (content.matches(wikip2)) {
      val wikip2r(wpath) = content
      WID.fromPath(wpath)
    } else None
  }

  /** todo should use this version instead of content - this resolves includes */
  def included : String = {
    // todo this is not cached as the underlying page may change - need to pick up changes
    var done = false

      val INCLUDE = """(?<!`)\[\[include:([^\]]*)\]\]""".r
      val res1 = INCLUDE.replaceAllIn(content, { m =>
        val other = for (
          wid <- WID.fromPath(m.group(1));
          c <- wid.content // this I believe is optimized for categories
        ) yield c

        done = true
        //regexp uses $ as a substitution, escape them before returning this subst string
        other.map(_.replaceAll("\\$", "\\\\\\$")).getOrElse("`[ERR Can't include $1]`")
      })

    if(done) res1 else content
  }

  def wid  = WID(category, name, parent, None, if(realm == Wikis.RK) None else Some(realm))
  def uwid = UWID(category, _id, if(realm == Wikis.RK) None else Some(realm))

  /** i should be conservative and default to rk. Note this doesn't check Config.urlcanon */
  def canonicalUrl =
    if (realm != Wikis.RK) {
      Wikis(realm).navTagFor(tags).map(x => s"http://www.racerkidz.com/wiki/${wid.wpath}") getOrElse s"http://www.racerkidz.com/wiki/${wid.wpath}"
    } else s"http://www.racerkidz.com/wiki/${wid.wpath}"

  def cloneContent(newcontent: String) = copy(content = newcontent)

  def cloneNewVer(label: String, markup: String, content: String, by: ObjectId, props: Map[String, String] = this.props) =
    copy(label=label, markup=markup, content=content, by=by, ver=ver + 1, props=props, updDtm=DateTime.now)

  def cloneProps(m: Map[String, String], sby: ObjectId) = copy(props = this.props ++ m)

  def withTags(s: Seq[String], sby: ObjectId) = copy(tags=s)

  def findParent = parent flatMap (p => Wikis(realm).find(p))

  def isReserved = props.get(PROP_RESERVED).exists(_ == "yes")

  def isPrivate = "User" == category || (props.exists(e => PROP_OWNER == e._1))
  def isOwner(id: String) = ("User" == category && name == id) || (props.exists(e => PROP_OWNER == e._1 && id == e._2))
  def owner = props.get(PROP_OWNER).flatMap(s => WikiUsers.impl.findUserById(new ObjectId(s)))

  // todo stupid name
  def getLabel = contentTags.getOrElse("label", label)
  def getDescription = contentTags.getOrElse("meta.description", getFirstParagraph.mkString)
  def getFirstParagraph = content.linesIterator.find(s => !s.trim.isEmpty && !".{".contains(s.trim.charAt(0)))

  def visibility = props.get(PROP_VISIBILITY).getOrElse(Visibility.PUBLIC)
  def wvis = props.get(PROP_WVIS).getOrElse(visibility)

  def create = {
    // TODO optimize exists
    if (Wikis.find(wid).exists(_.realm == this.realm)) {
      Log.error("ERR_WIKI page exists " + wid)
      throw new IllegalStateException(s"page already exists: $category/$name")
    }

    Audit.logdbWithLink(
      if(wid.cat=="Note") AUDIT_NOTE_CREATED else AUDIT_WIKI_CREATED,
      wid.urlRelative,
      "BY " + (WikiUsers.impl.findUserById(this.by).map(_.userName).getOrElse(this.by.toString)) +
        " " + category + ":" + name)
    Wikis(realm).weTable(wid.cat) += grater[WikiEntry].asDBObject(Audit.createnoaudit(this))
    Wikis.shouldFlag(name, label, content).map(auditFlagged(_))
    Wikis(realm).index.create(this)
  }

  /** backup old version and update entry, update index */
  def update(newVer: WikiEntry, reason:Option[String] = None)(implicit txn:Txn) = {
    Audit.logdbWithLink(
      if(wid.cat=="Note") AUDIT_NOTE_UPDATED else AUDIT_WIKI_UPDATED,
      newVer.wid.urlRelative,
      s"""BY ${(WikiUsers.impl.findUserById(newVer.by).map(_.userName).getOrElse(newVer.by.toString))} - $category : $name ver ${newVer.ver}""")
    WikiEntryOld(this, reason).create
    RUpdate.noAudit[WikiEntry](Wikis(realm).weTables(wid.cat), Map("_id" -> newVer._id), newVer)
    Wikis.shouldFlag(name, label, content).map(auditFlagged(_))

    if (shouldIndex) Wikis(realm).index.update(this, newVer)
  }

  /** should this entry be indexed in memory */
  def shouldIndex = !(Wikis.PERSISTED contains wid.cat)

  /** backup old version and update entry, update index */
  def delete(sby: String) (implicit txn:Txn) = {
    Audit.logdb(AUDIT_WIKI_DELETED, "BY " + sby + " " + category + ":" + name, "\nCONTENT:\n" + this)
    WikiEntryOld(this, Some ("deleted")).create
    WikiTrash("WikiEntry", this.grated, sby, txn.id).create
    val key = Map("realm" -> realm, "category" -> category, "name" -> name, "parent" -> parent)
    RDelete.apply (Wikis(realm).weTables(wid.cat), key)
    if (shouldIndex) Wikis(realm).index.delete(this)
  }

  def auditFlagged(f: String) { Log.audit(Audit.logdb(f, category + ":" + name)) }

  /** reparsing the content - wiki sections are delimited by {{section:name}} */

  /** these are normal - all sections after include */
  lazy val sections = findSections(included, PATT_SEC) ::: findSections(included, PATT_TEM)

  /** these are when used as a template - template sections do not resolve include */
  lazy val templateSections = findSections(included, PATT_SEC) ::: findSections(content, PATT_TEM)

  // this ((?>.*?(?=\{\{/))) means non-greedy lookahead
  //?s means DOTALL - multiline
  // format: {{stype[ :]name:signature}}
  private final val PATT_SEC =
    """(?s)\{\{\.*(section|def|lambda|dsl\.\w*)([: ])?([^:}]*)?(:)?([^}]*)?\}\}((?>.*?(?=\{\{/[^`])))\{\{/\.*(section|def|lambda|dsl\.\w*)?\}\}""".r
  private final val PATT_TEM =
    """(?s)\{\{\.*(template)([: ])?([^:}]*)?(:)?([^}]*)?\}\}((?>.*?(?=\{\{/[^`])))\{\{/\.*(template)?\}\}""".r

  /** find the sections - used because for templating I don't want to reolves the includes */
  private def findSections (c:String, pat:scala.util.matching.Regex) = {
    //todo use the wiki parser later modifiers to load the sections, not a separate parser here
    val PATT2 = pat

    (for (m <- pat.findAllIn(c)) yield {
      val mm = PATT2.findFirstMatchIn(m).get
      WikiSection(this, mm.group(1), mm.group(3), mm.group(5), mm.group(6))
    }).toList
  }

  /** pattern for all sections requiring signing - (?s) means multi-line */
  val PATTSIGN = """(?s)\{\{(template|def|lambda):([^:}]*)(:REVIEW[^}]*)\}\}((?>.*?(?=\{\{/)))\{\{/(template|def|lambda)?\}\}""".r //?s means DOTALL - multiline

  /** find a section */
  def section(stype: String, name: String) = sections.find(x => x.stype == stype && x.name == name)

  /** scripts are just a special section */
  lazy val scripts = sections.filter(x => "def" == x.stype || "lambda" == x.stype)

  /** pre processed form - parsed and graphed */
  lazy val ast = Wikis.preprocess(this.wid, this.markup, Wikis.noBadWords(this.content))

  /** pre processed form - parsed and graphed */
  lazy val preprocessed = {
    val s = ast.fold(WAST.context(Some(this))) // fold the AST
    // add hardcoded attribute - these can be overriden by tags in content
    WAST.SState(s.s,
      Map("category" -> category, "name" -> name, "label" -> label, "url" -> (wid.urlRelative),
      "id" -> _id.toString, "tags" -> tags.mkString(",")) ++ s.tags,
      s.ilinks)
  }

  def grated = grater[WikiEntry].asDBObject(this)

  override def toString: String =
    grater[WikiEntry].asDBObject(this).toString

  /** tags collected during parsing of the content, with some static tags like url,label etc */
  def contentTags = preprocessed.tags

  /** all the links from this page to others, based on parsed content */
  def ilinks = preprocessed.ilinks

  final val AUDIT_WIKI_CREATED = "WIKI_CREATED"
  final val AUDIT_WIKI_UPDATED = "WIKI_UPDATED"
  final val AUDIT_WIKI_DELETED = "WIKI_DELETED"
  final val AUDIT_NOTE_CREATED = "NOTE_CREATED"
  final val AUDIT_NOTE_UPDATED = "NOTE_UPDATED"

  /** field definitions contained - added to when accessing the form */
  var fields = new scala.collection.mutable.HashMap[String, FieldDef]()
  lazy val form = new WikiForm(this)
  def formRole = this.props.get(FormStatus.FORM_ROLE)

  /** other parsing artifacts to be used by knowledgeable modules.
    * Parsers can put stuff in here. */
  //todo move the fields and form stuff here
  val cache = new scala.collection.mutable.HashMap[String, Any]()
}

/** a form field definition */
case class FieldDef(name: String, value: String, attributes: Map[String, String]) {
  def withValue(x: String) = FieldDef(name, x, attributes)
}

/** a section inside a wiki page */
case class WikiSection(parent: WikiEntry, stype: String, name: String, signature: String, content: String) {
  def sign = Services.auth.sign(content)

  def checkSignature = Services.auth.checkSignature(sign, signature)

  def wid = parent.wid.copy(section=Some(name))

  override def toString = s"WikiSection(stype=$stype,name=$name,signature=$signature,content=$content)"
}

object WikiEntry {
  final val UPD_CONTENT = "UPD_CONTENT"
  final val UPD_TOGGLE_RESERVED = "UPD_TOGGLE_RESERVED"
  final val UPD_UOWNER = "UPD_UOWNER"

  final val PROP_VISIBILITY = "visibility"
  final val PROP_WVIS = "wvis"
  final val PROP_RESERVED = "reserved"
  final val PROP_OWNER: String = "owner"

  def grated(o: DBObject) = grater[WikiEntry].asObject(o)
}

/** old wiki entries - a copy of each older version when udpated or deleted */
@RTable
case class WikiEntryOld(entry: WikiEntry, reason:Option[String], _id: ObjectId = new ObjectId()) {
  def create (implicit txn:Txn) = RCreate.noAudit[WikiEntryOld](this)
}

/** old wiki entries - a copy of each older version when udpated or deleted */
@RTable
case class WikiTrash(table:String, entry: DBObject, by:String, txnId:String, date:DateTime=DateTime.now, _id: ObjectId = new ObjectId()) {
  def create (implicit txn:Txn) = RCreate.noAudit[WikiTrash](this)
}

