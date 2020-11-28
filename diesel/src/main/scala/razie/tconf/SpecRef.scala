/**   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package razie.tconf

/** uniquely identifies a piece of specification, an object or an asset
  *
  * for identifying sections inside a larger document, the wpath will include like a section pointer (...#section)
  *
  * source can be domain plugin and connection ID inside the target realm, "plugin:conn"
  * wpath can be at minimum "class:id" or qualified "realm.class:id"
  * wpath could include a section Id, like "realm.class:id#section"
  *
  * @param source - the source system: inventory understands and delegates to (preferably URL)
  * @param wpath  - unique id of the spec
  * @param realm  - optionally identify a realm within the source (multi-tenancy)
  * @param ver    - optionally identify a version of the spec
  * @param draft  - optionally identify a certain temporary variant (i.e. autosaved by username)
  */
trait TSpecRef {
  def source: String

  def wpath: String

  def realm: String

  def ver: Option[String]

  def draft: Option[String]

  def ahref: Option[String]
}

/** basic implmentation */
case class SpecRef(source: String,
                   wpath: String,
                   realm: String,
                   ver: Option[String] = None,
                   draft: Option[String] = None)
    extends TSpecRef {

  def ahref: Option[String] = None
}

/**
  * generic complete implementation for an asset ref
  *
  * (realm,plugin,conn) uniquely identify the domain and source
  * (cls,id,section,ver,draft) uniquely identify the asset inside there
  */
case class FullSpecRef(
  inventory: String,
  conn: String,
  cls: String,
  id: String,
  section: String,
  realm: String,
  ver: Option[String] = None,
  draft: Option[String] = None)

    extends TSpecRef {

  override def source: String = inventory + ":" + conn

  override def wpath: String = cls + ":" + id

  override def ahref: Option[String] = None
}

/** utilities */
object SpecRef {

  /** make a generic specref */
  def make(realm: String, inventory: String, conn: String, cls: String, id: String, section: String = "") = {
    new FullSpecRef(
      inventory,
      conn,
      cls,
      id,
      section,
      realm
    )
  }

  /** decompose into a full spec */
  def full(ref: TSpecRef) = {
    val s1 = ref.source.split(":")
    val s2 = ref.wpath.split(":")
    val inv = if (s1.length > 1) s1(0) else ""
    val conn = if (s1.length > 1) s1(1) else s1(0)
    var cls = if (s2.length > 1) s1(0) else ""
    var id = if (s2.length > 1) s1(0) else s2(0)
    var sec = ""

    if (cls.contains(".")) {
      cls = cls.split('.')(1)
    }

    if (id.contains("#")) {
      id = id.split('#')(0)
      sec = id.split('#')(1)
    }

    new FullSpecRef(
      inv,
      conn,
      cls,
      id,
      sec,
      ref.realm,
      ref.ver,
      ref.draft
    )
  }
}


