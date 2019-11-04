/*  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine.nodes

import razie.diesel.dom.RDOM.P
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** a single match, collected when looking for expectations */
class SingleMatch(val x: Any) {
  var score = 0;
  val diffs = new mutable.HashMap[String, (Any, Any)]() // (found, expected)
  val misses = new mutable.ArrayBuffer[String] // names didn't match

  def plus(s: String) = {
    score += 1
  }

  def minus(name: String, found: Any, expected: Any) = {
    diffs.put(name, (found.toString, expected.toString))
  }

  // missed opportunity to match this
  def missed(name: String) = {
    misses.append(name)
  }

  // missed opportunity to match this
  def missedValue(p: P) = {
    misses.append(p.toString)
  }
}

/** collects the intermediary tests for a match, when looknig for expectations */
class MatchCollector {
  var cur = new SingleMatch("")
  var highestScore = 0;
  var highestMatching: Option[SingleMatch] = None
  val old = new ListBuffer[SingleMatch]()

  def done = {
    if (cur.score >= highestScore) {
      highestScore = cur.score
      highestMatching = Some(cur)
    }
    old.append(cur)
  }

  def newMatch(x: Any) = {
    done
    cur = new SingleMatch(x)
  }

  def plus(s: String) = cur.plus(s)

  /** negative match
    *
    * @param name     the name I was looking for
    * @param found    the value I found
    * @param expected the value I expetected to find
    */
  def minus(name: String, found: Any, expected: Any) =
    cur.minus(name, found, expected)

  // record missed opportunity to match (names did not match)
  def missed(name: String) =
    cur.missed(name)

  // record missed opportunity to match (names did not match)
  def missedValue(p: P) =
    cur.missedValue(p)

  def toHtml =
    highestMatching
        .map { h =>
          h.diffs.values.map(_._1)
              .toList
              .map(x => s"""<span style="color:red">${htmlValue(x.toString)}</span>""")
              .mkString(",") +
              (
                  // todo list them in order of close to far (i.e. a close name)
                  if (h.misses.size > 0)
                    "found: " + h.misses
                        .map(x => s"""<span style="color:red">${htmlValue(x)}</span>""")
                        .mkString(",")
                  else ""
                  )
        }
        .mkString
}


