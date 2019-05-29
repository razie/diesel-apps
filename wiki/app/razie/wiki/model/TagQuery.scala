package razie.wiki.model

import com.mongodb.DBObject
import com.mongodb.casbah.Imports._
import razie.wiki.model.Visibility.PUBLIC

/**
  * important concept - query/select a list of topics, based on inclusion/exclusion of tags
  *
  * LOGICAL: a/b|c/d is a and (b or c) and d
  * ACTUAL:  a/b,c/d is a and (b or c) and d     - in url , is not escaped, so easier to use than |
  *
  * todo perf if tq contains a cat like below, optimize to search just those - it's done in DomGuardian
  *
  * tag query tricks: if a tag uses an upper case like "Story" then it referes
  * to the category and it optimizes things a lot in big reactors
  *
  * ltags - all AND expressions
  * atags - the AND expressions without OR
  * otags
  * qt    - array of array - first is AND second is OR
  *
  * NOTE the one way to do the search today is WikiSearch.getList
  */
class TagQuery(val tags: String) {
  val ltags = tags.split("/").map(_.trim).filter(goodTag)
  val atags = ltags.filter(_.indexOf(",") < 0).map(_.toLowerCase)
  val otags = ltags.filter(_.indexOf(",") >= 0).map(_.split(",").map(_.trim.toLowerCase).filter(goodTag))
  val newqt = ltags.map(_.split(",").map(_.toLowerCase).filter(goodTag))

  /** create a new tags addinng the t */
  def and (t:String) = {
    val s = if(tags.trim.length > 0) tags + "," + t else t
    new TagQuery(t)
  }

  // array of array - first is AND second is OR
  val qt = ltags.map(_.split(",").filter(goodTag))

  def goodTag(x:String) = x.length > 0 && x != "tag"

  // can't mix public with something else and still get public...
  def public = ltags contains "public"

  /** check if a set of tags matches */
  def matches(t: Seq[String]) = {
    atags.foldLeft(true)((a, b) => a &&
      (if (b.startsWith("-")) !t.contains(b.substring(1))
      else t.contains(b))
    ) &&
      otags.foldLeft(true)((a, b) => a &&
        b.foldLeft(false)((a, c) => a || t.contains(c))
      )
  }

  def matches (u:DBObject) = {
    val utags = if(u.containsField("tags")) u.get("tags").toString.toLowerCase else ""

    def checkT(b:String) = {
      utags.contains(b) ||
        b == "*" ||
        (b == "draft" && u.containsField("props") && u.getAs[DBObject]("props").exists(_.containsField("draft"))) ||
        (b == "public" && u.containsField("props") && u.getAs[DBObject]("props").exists(_.getAsOrElse[String]("visibility", PUBLIC) == PUBLIC)) ||
        u.get("category").toString.toLowerCase == b
    }

    qt.size <= 0 ||
      u.containsField("tags") &&
      qt.foldLeft(true)((a, b) => a && (
        if(b(0).startsWith("-")) ! checkT(b(0).substring(1))
        else b.foldLeft(false)((a, b) => a || checkT(b))
        ))
  }

  def matches (u:WikiEntry) = {
    val utags = u.tags.mkString

    def checkT(b: String) = {
      utags.contains(b) ||
        b == "*" ||
        (b == "draft" && u.isDraft) ||
        (b == "public" && u.visibility == PUBLIC) ||
        u.category.toLowerCase == b
    }

    qt.size <= 0 ||
      u.tags.size > 0 &&
      qt.foldLeft(true)((a, b) => a && (
        if (b(0).startsWith("-")) !checkT(b(0).substring(1))
        else b.foldLeft(false)((a, b) => a || checkT(b))
        ))
  }

  /** is any of the t list of tags included in this query?
    *
    * used to count tags of a result not in the query
    */
  def contains(t: String) = {
    atags.foldLeft(false)((a, b) => a || t.contains(b)) ||
      otags.foldLeft(false)((a, b) => a ||
        b.foldLeft(false)((a, c) => a || t.contains(c))
      )
  }
}

