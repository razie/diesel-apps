package model

/** simple model for tags */
object Tags {
  type Tags = Seq[(String, Int)]
  def empty = Seq.empty[(String, Int)]

  final val ARCHIVE = "archive"
  final val ALL = "all"
  final val NONE = "none"
  final val RECENT = "recent"
  final val INBOX = "inbox"

  def apply (s:String) = s.split(",").map(_.trim).filter(_.length > 0).toSeq
}


