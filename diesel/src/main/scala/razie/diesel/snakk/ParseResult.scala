/**
  *  ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package razie.diesel.snakk

/** either a result, or a list of errors or both */
case class ParseResult[A] (private var result:Option[A], private var errors:List[String]) {
  def hasErrors = errors.nonEmpty
  def hasResult = result.nonEmpty
  def getErrors = errors
  def getResult = result

  def collectError(e:String) = {
    errors = e :: errors
    this
  }

  def collectResult(r:A) = {
    if (result.isDefined) throw new IllegalStateException("result already collected")
    else result = Some(r)
    this
  }

}

object ParseResult {
  def apply [A] (result:A) = new ParseResult(Some(result), Nil)
  def apply [A] (errors:List[String]) = new ParseResult(None, errors)
  def error [A] (error:String) = new ParseResult(None, List(error))
  def empty [A] = new ParseResult[A](None, Nil)
}

