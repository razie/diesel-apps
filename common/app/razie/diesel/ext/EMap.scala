/**
 *  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.ext

import razie.diesel.dom.RDOM._
import razie.diesel.dom._

import scala.Option.option2Iterable
import scala.util.Try

/** mapping a message - a decomposition rule (right hand side of =>)
  *
  * @param cls
  * @param met
  * @param attrs
  */
case class EMap(cls: String, met: String, attrs: Attrs, arrow:String="=>", cond: Option[EIf] = None) extends CanHtml {

  // todo this is rather stupid - need better accounting
  /** count the number of applications of this rule */
  var count = 0;

  def apply(in: EMsg, destSpec: Option[EMsg], pos:Option[EPos])(implicit ctx: ECtx): List[Any] = {
    var e = Try {
      val m = EMsg("generated", cls, met, sourceAttrs(in, attrs, destSpec.map(_.attrs))).
        withPos(pos).
        withSpec(destSpec)

      if(arrow == "==>" || cond.isDefined)
        ENext(m, arrow, cond)
      else m
    }.recover {
      case t:Throwable => {
        razie.Log.log("trying to source message", t)
        EError(t.getMessage, t.toString)
      }
    }.get
    count += 1

    List(e)
  }

  def sourceAttrs(in: EMsg, spec: Attrs, destSpec: Option[Attrs])(implicit ctx: ECtx) = {
    // current context, msg overrides
    val myCtx = new StaticECtx(in.attrs, Some(ctx))

    // solve an expression
    def expr(p: P) = {
      p.expr.map(_.apply("")(myCtx)/*.toString*/).getOrElse{
        val s = p.dflt
        s
        //          if (s.matches("[0-9]+")) s // num
        //          else if (s.startsWith("\"")) s // string
        //          else in.attrs.find(_.name == s).map(_.dflt).getOrElse("")
      }
    }

    if (spec.nonEmpty) spec.map { p =>
      // sourcing has expr, overrules
      val v =
        if(p.dflt.length > 0 || p.expr.nonEmpty) expr(p)
        else in.attrs.find(_.name == p.name).map(_.dflt).orElse(
          ctx.get(p.name)
        ).getOrElse(
          "" // todo or somehow mark a missing parm?
        )

      val tt =
        if (p.ttype.isEmpty && !p.expr.exists(_.getType != "") && v.isInstanceOf[Int]) WTypes.NUMBER
        else if (p.ttype.isEmpty) p.expr.map(_.getType).mkString
        else p.ttype

      p.copy(dflt = v.toString, ttype=tt)
    } else if (destSpec.exists(_.nonEmpty)) destSpec.get.map { p =>
      // when defaulting to spec, order changes
      val v = in.attrs.find(_.name == p.name).map(_.dflt).orElse(
        ctx.get(p.name)
      ).getOrElse(
        expr(p)
      )
      val tt = if(p.ttype.isEmpty && v.isInstanceOf[Int]) WTypes.NUMBER else ""
      p.copy(dflt = v.toString, expr=None, ttype=tt)
    } else {
      // if no map rules and no spec, then just copy/propagate all parms
      in.attrs.map {a=>
        a.copy()
      }
    }
  }

  def asMsg = EMsg("", cls, met, attrs.map{p=>
    P (p.name, p.dflt, p.ttype, p.ref, p.multi)
  })

//  override def toHtml = "<b>=&gt;</b> " + ea(cls, met) + " " + attrs.map(_.toHtml).mkString("(", ",", ")")
  override def toHtml = """<span class="glyphicon glyphicon-arrow-right"></span> """ + cond.map(_.toHtml+" ").mkString + ea(cls, met) + " " + toHtmlAttrs(attrs)

  override def toString = "=> " + cond.map(_.toHtml+" ").mkString + cls + "." + met + " " + attrs.mkString("(", ",", ")")
}
