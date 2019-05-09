/**
  *   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 **/
package razie.tconf

/** helpers common to spec parsers */
package object parser {

  final val T_PREPROCESS = "pre"
  final val T_VIEW = "view"
  final val T_TEMPLATE = "template"

  implicit def toSState(s: String): BaseAstNode = StrAstNode(s)
  implicit def toLState(s: Seq[BaseAstNode]): BaseAstNode = s match {
    // optimixze empty lists away
    case x :: Nil if (x.isInstanceOf[ListAstNode]) =>
      x.asInstanceOf[ListAstNode]
    case x :: Nil => x
    case _        => ListAstNode(s: _*)
  }

}
