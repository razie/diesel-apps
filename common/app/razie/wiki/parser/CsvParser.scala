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

  def csvLine(implicit xdelim: String): PS1 = (cell | xdelim) ~ rep(xdelim ~ (cell | NADA)) ^^ {
    case ol ~ l => {
      if (ol == xdelim) List("") ::: l.map(_._2)
      else List(ol) ::: l.map(_._2)
    }
  }

  def csvLines(implicit xdelim: String): PS2 = rep(csvOptline ~ (CRLF1 | CRLF3 | CRLF2)) ~ opt(csvLine) ^^ {
    case l ~ c => (l.map(_._1) ::: c.toList)
  }

  def plainLines(implicit end: String): P = rep(opt(plainLine) ~ (CRLF1 | CRLF3 | CRLF2)) ~ opt(plainLine) ^^ {
    case l ~ c => l.flatMap(_._1.map(_ + "\n")).mkString + c.getOrElse("")
  }

  def plainLine(implicit end: String): P = (not(end) ~> """.""".r+) ^^ { case l => l.mkString }

  def csvOptline(implicit xdelim: String): PS1 = opt(csvLine) ^^ { case o => o.map(identity).getOrElse(Nil) }

  def cell(implicit xdelim: String): P = (not(xdelim) ~> not("{{") ~> not("}}") ~> """.""".r+) ^^ { case l => l.mkString }
}


