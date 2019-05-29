/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.parser

import razie.tconf.parser.JMapFoldingContext
import razie.wiki.model.{WikiEntry, WikiUser}
import razie.tconf.parser._

/**
  * wiki AST - abstract syntax tree.
  *
  * 1. Wikis are parsed in AST
  * 2. then folded into markdown with a context
  * 3. then markdown is turned into html via markdown parser
  */
object WAST {
  private def toMap (we:Option[WikiEntry]) = we.map(w=>Map(
    "category" -> w.category,
    "name" -> w.name,
    "wpath"->w.wid.wpath
  )).getOrElse(Map.empty)

  /** create a normal context - same as VIEW */
  def context         (we:Option[WikiEntry], au:Option[WikiUser]=None, ctx:Map[String,Any]=Map.empty) =
    new JMapFoldingContext(we, au, T_PREPROCESS, ctx ++ toMap(we))

  /** create a context for view */
  def contextView     (we:Option[WikiEntry], au:Option[WikiUser]=None, ctx:Map[String,Any]=Map.empty) =
    new JMapFoldingContext(we, au, T_VIEW, ctx ++ toMap(we))

  /** create a special context for templates - template expressions are expanded only */
  def contextTemplate (we:Option[WikiEntry], au:Option[WikiUser]=None, ctx:Map[String,Any]=Map.empty) =
    new JMapFoldingContext(we, au, T_TEMPLATE, ctx ++ toMap(we))

}
