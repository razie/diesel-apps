package razie.wiki.model

/**
  * important concept - query/select a list of topics, based on inclusion/exclusion of tags
  *
  *  a/b|c/d is a and (b or c) and d
  *
  * tag query tricks: if a tag uses an upper case like "Story" then it referes
  * to the category and it optimizes things a lot in big reactors
  */
class TagQuery(val tags: String) {
  val ltags = tags.split("/").map(_.trim).filter(goodTag)
  val atags = ltags.filter(_.indexOf(",") < 0).map(_.toLowerCase)
  val otags = ltags.filter(_.indexOf(",") >= 0).map(_.split(",").map(_.trim.toLowerCase).filter(goodTag))
  val qt = ltags.map(_.split(",").map(_.toLowerCase).filter(goodTag))

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

