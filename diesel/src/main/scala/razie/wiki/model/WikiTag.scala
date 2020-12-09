/**
  * .____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.wiki.model

import org.bson.types.ObjectId
import razie.db.{REntity, ROne, RTable}
import razie.hosting.WikiReactors

/** a versioned wiki - using ID so this works even if names change later */
case class VUWID(uwid: UWID, ver: String)

/**
  * todo WIP
  * todo see also the wikiTag.scala.xml
  *
  * tagging a reactor - will record the versions of each page present at that time
  *
  * use cases: create a tag, retrieve a tag, reference a tag
  *
  * this corresponds to a wiki of WikiTag:name
  */
@RTable
case class WikiTag(
  realm: String,
  name: String,
  pages: List[VUWID],
  _id: ObjectId = new ObjectId()
) extends REntity[WikiTag] {

  override def toString = s"WikiTag ($realm) $name"
}

object WikiTag {

  def create(realm: String, name: String): WikiTag = {
    val pages = listPages(realm)
    val t = WikiTag(realm, name, pages.toList)
    t.create(razie.db.tx.auto)
    t
  }

  // update to current versions
  def update(realm: String, name: String): WikiTag = {
    find(realm, name).map { wtag =>
      val pages = listPages(realm)
      val t = wtag.copy(pages = pages)
      t.update(razie.db.tx.auto)
      t
    }.getOrElse {
      create(realm, name)
    }
  }

  private def listPages(realm: String): List[VUWID] = {
    val specs = Wikis(realm).pages("*").map(w => VUWID(w.uwid, w.ver.toString))
    specs.toList ++ WikiReactors(realm).mixins.flattened.map(_.realm).flatMap(listPages)
  }

  def find(realm: String, name: String): Option[WikiTag] = ROne[WikiTag]("realm" -> realm, "name" -> name)
}

