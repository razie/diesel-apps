/**   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package razie.tconf

/** uniquely identifies a piece of specification
  *
  * for identifying sections inside a larger document, the wpath will include like a section pointer (...#section)
  *
  * @param source - the source system: inventory understands and delegates to
  * @param wpath - unique id of the spec
  * @param realm - optionally identify a realm within the source (multi-tenancy)
  * @param ver - optionally identify a version of the spec
  * @param draft - optionally identify a certain temporary variant (i.e. autosaved by username)
  */
trait TSpecPath {
  def source: String
  def wpath: String
  def realm: String
  def ver: Option[String]
  def draft: Option[String]
  def ahref: Option[String]
}

/** basic implmentation */
case class SpecPath(source: String,
                    wpath: String,
                    realm: String,
                    ver: Option[String] = None,
                    draft: Option[String] = None)
    extends TSpecPath {

  def ahref: Option[String] = None
}


