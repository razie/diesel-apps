/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.model

import org.bson.types.ObjectId
import razie.db.{REntity, ROne, RTable}

case class VUWID (uwid:UWID, ver:String)

/**
  * tagging a reactor - will record the versions of each page present at that time
  *
  * use cases: create a tag, retrieve a tag, reference a tag
  */
@RTable
case class WTag(
  realm: String,
  name: String,
  list:List[VUWID],
  _id : ObjectId = new ObjectId()
  ) extends REntity[WTag] {

  override def toString = s"WTag ($realm) $name"
}

object WTag {

  def create (realm:String, name:String) :WTag = {
    val specs = Wikis(realm).pages("*").map(w=> VUWID(w.uwid, w.ver.toString))
    return WTag (realm, name, specs.toList)
  }

  def find (realm:String, name:String) : Option[WTag] = ROne[WTag]("realm" -> realm, "name" -> name)

}
