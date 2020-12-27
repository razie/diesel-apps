/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.parser

/** delimited and table parser */
trait CsvParser extends ParserCommons {

  def csv(implicit xdelim: String): Parser[List[List[String]]] = csvLines

  def csvCRLF2: PS2 = CRLF2 ^^ { case x => Nil }

  def csvNADA: PS2 = NADA ^^ { case x => Nil }

  def parseDelim(implicit xdelim: String) = (" *" + xdelim).r

  def oldcsvLine(implicit xdelim: String): PS1 = (csvstrConst | cell | xdelim) ~ rep(
    xdelim ~ (csvstrConst | cell | NADA)) ^^ {
    case ol ~ l => {
//       doesn't work without emptyFirstCell - no clue why
      if (ol == xdelim || ol == "") List("") ::: l.map(_._2)
      else List(ol) ::: l.map(_._2)
    }
  }

  def csvLine(implicit xdelim: String): PS1 = repsep((csvstrConst | cell | NADA), parseDelim) ^^ {
    case l => {
      l.filter(_.nonEmpty)
    }
  }

//  def emptyLine(implicit xdelim: String): PS1 = NADA ^^ {
//    case ol ~ l => {
//      if (ol == xdelim) List("") ::: l.map(_._2)
//      else List(ol) ::: l.map(_._2)
//    }
//  }

  def csvLines(implicit xdelim: String): PS2 = rep(csvOptline ~ (CRLF1 | CRLF3 | CRLF2)) ~ opt(csvLine) ^^ {
    case l ~ c => (l.map(_._1) ::: c.toList)
  }

  def plainLines(implicit end: String): P = rep(opt(plainLine) ~ (CRLF1 | CRLF3 | CRLF2)) ~ opt(plainLine) ^^ {
    case l ~ c => l.flatMap(_._1.map(_ + "\n")).mkString + c.getOrElse("")
  }

  def plainLine(implicit end: String): P = (not(end) ~> """.""".r +) ^^ { case l => l.mkString }

  def csvOptline(implicit xdelim: String): PS1 = opt(csvLine) ^^ { case o => o.map(identity).getOrElse(Nil) }

  def emptyFirstCell(implicit xdelim: String): P = (xdelim) ^^ {
    case l => ""
  }

  def cell(implicit xdelim: String): P = (not(xdelim) ~> not("{{") ~> not("}}") ~> """.""".r +) ^^ {
    case l => l.mkString.trim
  }

  // string const with escaped chars
  // this is specific to csv, the leading spaces... don't reuse this to parse strings with that in
  def csvstrConst: Parser[String] = " *\"".r ~> """(\\.|[^\"])*""".r <~ "\"" ^^ {
    e => csvprepStrConst(e.trim)
  }

  // a number
  def csvnumConst: Parser[Any] = (csvafloat | csvaint | csvabool) ^^ { case i => {i} }

  def csvaint: Parser[Int] = """-?\d+""".r ^^ { case x => x.toInt }

  def csvafloat: Parser[Float] = """-?\d+[.]\d+""".r ^^ { case x => x.toFloat }

  def csvabool: Parser[Boolean] = ("true" | "false") ^^ { case x => x.toBoolean }

  /** prepare a parsed string const */
  private def csvprepStrConst(e: String) = {
    // string const with escaped chars
    var s = e

    // replace standard escapes like java does
    s = s
        .replaceAll("(?<!\\\\)\\\\b", "\b")
        .replaceAll("(?<!\\\\)\\\\n", "\n")
        .replaceAll("(?<!\\\\)\\\\t", "\t")
        .replaceAll("(?<!\\\\)\\\\r", "\r")
        .replaceAll("(?<!\\\\)\\\\f", "\f")

    // kind of like java, now replace anything escaped
    // note that Java only replaces a few things, others generate errors
    // we replace anything

//    s = s.replaceAll("\\\\(.)", "$1")

    if (!s.startsWith("\""))
      "\"" + s + "\""
    else
      s
  }
}

