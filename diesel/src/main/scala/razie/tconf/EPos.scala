/**   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package razie.tconf

import scala.collection.mutable.HashMap

/** position of an element - reference where the item was defined, so we can scroll back to it */
case class EPos(wpath: String, line: Int, col: Int) {
  def this(o: scala.collection.Map[String, Any]) =
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

