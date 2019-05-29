/**
 *  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.ext

import razie.diesel.dom.RDOM._
import razie.diesel.dom._
import razie.tconf.EPos
import scala.Option.option2Iterable
import scala.util.Try

object EMap {

  def sourceAttrs(in: EMsg, spec: Attrs, destSpec: Option[Attrs], deferEvaluation:Boolean=false)(implicit ctx: ECtx) = {
    // current context, msg overrides
    val myCtx = new StaticECtx(in.attrs, Some(ctx))

    // solve an expression
    def expr(p: P) = {
      p.expr.map(_.applyTyped("")(myCtx)/*.toString*/).getOrElse{
        // need to preserve types and stuff
        p
      }
    }

    val out1 = if (spec.nonEmpty) spec.map { p =>
      if(deferEvaluation)
        p
      else { // do evaluation now
        // sourcing has expr, overrules
        val v =
          if(p.dflt.length > 0 || p.expr.nonEmpty) Some(expr(p)) // a=x
          else in.attrs.find(_.name == p.name).orElse( // just by name: no dflt, no expr
            ctx.getp(p.name)
          )

        val tt =
          v.map(_.ttype).getOrElse {
            if (p.ttype.isEmpty && !p.expr.exists(_.getType != "") && (v.isInstanceOf[Int] || v.isInstanceOf[Float])) WTypes.NUMBER
            else if (p.ttype.isEmpty) p.expr.map(_.getType).mkString
            else p.ttype
          }

        p.copy(dflt = v.map(_.dflt).mkString, ttype=tt, value = v.flatMap(_.value))
      }
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

  def apply(in: EMsg, destSpec: Option[EMsg], apos:Option[EPos], deferEvaluation:Boolean=false, arch:String = "")(implicit ctx: ECtx): List[Any] = {

    val kind = AstKinds.kindOf(arch)

    var e = Try {
      val m = EMsg(
         cls, met,
        EMap.sourceAttrs(in, attrs, destSpec.map(_.attrs), deferEvaluation),
        kind)
        .withPos(this.pos.orElse(apos))
        .withSpec(destSpec)

      if(arrow == "==>" || cond.isDefined || deferEvaluation)
        ENext(m, arrow, cond, deferEvaluation).withParent(in).withSpec(destSpec)
      else m
    }.recover {
      case t:Throwable => {
        razie.Log.log("trying to source message", t)
        new EError("Exception trying to source message", t)
        }
    }.get
    count += 1

    List(e)
  }

  def asMsg = EMsg(cls, met, attrs.map{p=>
    P (p.name, p.dflt, p.ttype, p.ref, p.multi)
  })

//  override def toHtml = "<b>=&gt;</b> " + ea(cls, met) + " " + attrs.map(_.toHtml).mkString("(", ",", ")")
  override def toHtml = """<span class="glyphicon glyphicon-arrow-right"></span> """ + cond.map(_.toHtml+" ").mkString + ea(cls, met) + " " + toHtmlAttrs(attrs)

  override def toString = "=> " + cond.map(_.toHtml+" ").mkString + cls + "." + met + " " + attrs.mkString("(", ",", ")")
}
