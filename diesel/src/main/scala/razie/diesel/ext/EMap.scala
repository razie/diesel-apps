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

object EMap {

  def sourceAttrs(in: EMsg, spec: Attrs, destSpec: Option[Attrs], deferEvaluation:Boolean=false)(implicit ctx: ECtx) = {
    // current context, msg overrides
    val myCtx = new StaticECtx(in.attrs, Some(ctx))

    // solve an expression
    def expr(p: P) = {
      p.expr.map(_.applyTyped("")(myCtx)/*.toString*/).getOrElse{
        P("", p.dflt)
      }
    }

    val out1 = if (spec.nonEmpty) spec.map { p =>
      val pe =
        if(p.dflt.length > 0 || p.expr.nonEmpty) Some(p)
        else in.attrs.find(_.name == p.name).orElse(
          ctx.getp(p.name)
        )

      // sourcing has expr, overrules
      val v =
        if(p.dflt.length > 0 || p.expr.nonEmpty) Some(expr(p))
        else in.attrs.find(_.name == p.name).orElse(
          ctx.getp(p.name)
        )

      val tt =
        v.map(_.ttype).getOrElse {
          if (p.ttype.isEmpty && !p.expr.exists(_.getType != "") && v.isInstanceOf[Int]) WTypes.NUMBER
          else if (p.ttype.isEmpty) p.expr.map(_.getType).mkString
          else p.ttype
        }

      if(deferEvaluation)
        p
      else
        p.copy(dflt = v.map(_.dflt).mkString, ttype=tt)
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

    out1
  }

}

/** mapping a message - a decomposition rule (right hand side of =>)
  *
  * @param cls
  * @param met
  * @param attrs
  */
case class EMap(cls: String, met: String, attrs: Attrs, arrow:String="=>", cond: Option[EIf] = None) extends CanHtml with HasPosition {
  var pos: Option[EPos] = None

  def withPosition (p:EPos) = { this.pos=Some(p); this}

  // todo this is rather stupid - need better accounting
  /** count the number of applications of this rule */
  var count = 0;

  def apply(in: EMsg, destSpec: Option[EMsg], apos:Option[EPos], deferEvaluation:Boolean=false)(implicit ctx: ECtx): List[Any] = {
    var e = Try {
      val m = EMsg(
        "generated", cls, met,
        EMap.sourceAttrs(in, attrs, destSpec.map(_.attrs), deferEvaluation)
      ).
        withPos(this.pos.orElse(apos)).
        withSpec(destSpec)

      if(arrow == "==>" || cond.isDefined || deferEvaluation)
        ENext(m, arrow, cond, deferEvaluation).withParent(in).withSpec(destSpec)
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

  def asMsg = EMsg("", cls, met, attrs.map{p=>
    P (p.name, p.dflt, p.ttype, p.ref, p.multi)
  })

//  override def toHtml = "<b>=&gt;</b> " + ea(cls, met) + " " + attrs.map(_.toHtml).mkString("(", ",", ")")
  override def toHtml = """<span class="glyphicon glyphicon-arrow-right"></span> """ + cond.map(_.toHtml+" ").mkString + ea(cls, met) + " " + toHtmlAttrs(attrs)

  override def toString = "=> " + cond.map(_.toHtml+" ").mkString + cls + "." + met + " " + attrs.mkString("(", ",", ")")
}
