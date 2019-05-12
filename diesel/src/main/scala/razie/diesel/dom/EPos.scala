package razie.diesel.dom

/** position of an element - reference where the item was defined, so we can scroll back to it */
case class EPos(wpath: String, line: Int, col: Int) {
  def this(o: Map[String, Any]) =
    this(
      o.getOrElse("wpath", "").toString,
      o.getOrElse("line", "0").toString.toInt,
      o.getOrElse("col", "0").toString.toInt
    )

  def toJmap = Map("wpath" -> wpath, "line" -> line, "col" -> col)

  /** points to nothing */
  def isEmpty = {
    line == -1 && col == -1 && wpath.isEmpty
  }

  override def toString = s"""{wpath:"$wpath", line:$line, col:$col}"""
  def toRef = s"""weref('$wpath', $line, $col)"""
}

object EPos {
  val EMPTY = EPos("", -1, -1)
}

