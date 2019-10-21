/**   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \          Read
  *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.dom

import razie.Debug._
import razie._
import razie.audit.Audit
import razie.diesel.dom.RDOM.P
import razie.diesel.expr.Expr

/** an xpath expr */
case class XPathIdent(val expr: String) extends Expr {
  override def apply(v: Any)(implicit ctx: ECtx) = ctx.apply(expr)
  override def applyTyped(v: Any)(implicit ctx: ECtx): P = ???

  private def xpRoot (p:Option[P]) = {
    p
  }

  def xpl(path: String, p: Option[P] = None) = {
    (for (
      worig <- xpRoot(p)
    ) yield {
      val root = new razie.Snakk.Wrapper(new DASWrapper(worig), DASXpSolver)

      Audit.logdb("XP-L", p.mkString + "/xpl/" + path)

      val xpath = "*/" + path
      val res = (root xpl xpath).collect {
        case we: DASWrapper => we
      }

//      val tags = res.flatMap(_._3).filter(_ != "").groupBy(identity).map(t => (t._1, t._2.size)).toSeq.sortBy(_._2).reverse
     res
    }) getOrElse
        Nil
  }

}

/** base class - can have a topic wrapper or a link wrapper */
class DASWrapper(val p:P) {

  /** special node means all */
  def isAll = "*" == p.name

  def childrenByTag (tag:String) = {
    if (isAll && "*" == tag)
      Nil
    else if (isAll)
      Nil
    else
      Nil
  }

  def getElem(name:String) = {
    val v = p.value
    if(v.exists(_.contentType == WTypes.JSON)) {
      v.get.asJson.get(name)
    } else {
      None
    }
  }

  def getAttr(name:String) = getElem(name).mkString

}

/**
  *  solver for wiki xp
  */
object DASXpSolver extends XpSolver[DASWrapper] {

  type T = DASWrapper
  type CONT = PartialFunction[(String, String), List[DASWrapper]]  // getNext
  type U = CONT

  val debug = true

  override def children(root: T, xe:Option[XpElement]): (T, U) = (root, {
    case (tag, assoc) if root.isInstanceOf[DASWrapper] =>
      children2(root, tag).toList.teeIf(debug,"C").toList
  })

  override def getNext(o: (T, U), tag: String, assoc: String, xe:Option[XpElement]): List[(T, U)] = {
    if(debug) println("-getNext ("+o.toString+") ("+tag+")")
    // 1. apply continuation and filter just in case... all children of type
    o._2.apply(tag, assoc).filter(zz => XP.stareq(zz.p.ttype.name, tag)).teeIf(debug,"D").map(children(_, xe)).tee("E").toList
  }

  private def children2(node: DASWrapper, tag: String): Seq[DASWrapper] = {
    if(debug) println("---CHILDREN2 ("+node+") ("+tag+")")
    Nil
  }

  override def getAttr(o: T, attr: String): String = {
    if(debug) println("-getAttr ("+o.toString+") ("+attr+")")
    o.getAttr(attr)
  }

  override def reduce(curr: Iterable[(T, U)], xe: razie.XpElement): Iterable[(T, U)] =
    (xe.cond match {
      case null => curr.asInstanceOf[List[(T, U)]]
      case _    => curr.asInstanceOf[List[(T, U)]].filter(x => xe.cond.passes(x._1, this))
    }).filter(gaga => XP.stareq(gaga._1.asInstanceOf[DASWrapper].p.ttype.name, xe.name))
}


