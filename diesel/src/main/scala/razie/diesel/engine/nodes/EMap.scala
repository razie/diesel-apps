/*  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine.nodes

import razie.diesel.dom.RDOM._
import razie.diesel.dom._
import razie.diesel.engine.AstKinds
import razie.diesel.expr.{AExprIdent, ECtx, Expr, SimpleExprParser, StaticECtx}
import razie.tconf.EPos
import scala.Option.option2Iterable
import scala.util.Try

/** assignment - needed because the left side is more than just a val */
case class PAS (left:AExprIdent, right:Expr) extends CanHtml {
  override def toHtml = left.toHtml + "=" + right.toHtml
  override def toString = left.toString + "=" + right.toString
}

/** a mapping, the right side of '=>' */
abstract class EMap extends CanHtml with HasPosition {
  var pos: Option[EPos] = None
  def withPosition (p:EPos) : EMap

  def apply(in: EMsg, destSpec: Option[EMsg], apos:Option[EPos], deferEvaluation:Boolean=false, arch:String = "")(implicit ctx: ECtx): List[Any]

  // todo this is rather stupid - need better accounting
  /** count the number of applications of this rule */
  var count = 0;

  def cls:String
  def met:String

  def indent(indentLevel:Int, s:String = "&nbsp;&nbsp;") = Range(0, indentLevel+1).map(x=> s).mkString
}

/** special case of map, just assigning exprs, no message decomp
  *
  * @param attrs
  * @param arrow
  * @param cond
  */
case class EMapPas(attrs: List[PAS], arrow:String="=>", cond: Option[EIf] = None, indentLevel:Int=0) extends EMap {
  override def cls:String = ""
  override def met:String = ""

  override def withPosition (p:EPos) = { this.pos=Some(p); this}

  override def apply(in: EMsg, destSpec: Option[EMsg], apos:Option[EPos], deferEvaluation:Boolean=false, arch:String = "")(implicit ctx: ECtx): List[Any] = {

    var e = Try {
      val m = EMsgPas(
        EMap.sourcePasAttrs(attrs, deferEvaluation)
      )
          .withPos(this.pos.orElse(apos))

      // these evaluate right away, so no need for deferred
      ENextPas(m, arrow, cond, deferEvaluation, indentLevel)
          .withParent(in)
          .withSpec(destSpec)
    }.recover {
      case t:Throwable => {
        razie.Log.log("trying to source message", t)
        new EError("Exception trying to source message", t)
      }
    }.get
    count += 1

    List(e)
  }

  //  override def toHtml = "<b>=&gt;</b> " + ea(cls, met) + " " + attrs.map(_.toHtml).mkString("(", ",", ")")
  override def toHtml = indent(indentLevel) + """<span class="glyphicon glyphicon-arrow-right"></span> """ + cond.map(_.toHtml+" ").mkString + ea("", "") + " " + toHtmlPAttrs(attrs)

  override def toString = indent(indentLevel, " ") + "=> " + cond.map(_.toHtml+" ").mkString + ". " + attrs.mkString("(", ",", ")")
}

/** mapping a message - a decomposition rule (right hand side of =>)
  *
  * @param cls
  * @param met
  * @param attrs name = expr
  */
case class EMapCls(cls: String, met: String, attrs: Attrs, arrow:String="=>", cond: Option[EIf] = None, indentLevel:Int = 0) extends EMap {
  override def withPosition (p:EPos) = { this.pos=Some(p); this}

  override def apply(in: EMsg, destSpec: Option[EMsg], apos:Option[EPos], deferEvaluation:Boolean=false, arch:String = "")(implicit ctx: ECtx): List[Any] = {

    val kind = AstKinds.kindOf(arch)

    var e = Try {
      val m = EMsg(
        cls, met,
        EMap.sourceAttrs(in, attrs, destSpec.map(_.attrs), deferEvaluation),
        kind)
          .withPos(this.pos.orElse(apos))
          .withSpec(destSpec)

      if(arrow == "==>" || cond.isDefined || deferEvaluation)
        ENext(m, arrow, cond, deferEvaluation, indentLevel)
            .withParent(in)
            .withSpec(destSpec)
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
    P (p.name, p.currentStringValue, p.ttype)
  })

  //  override def toHtml = "<b>=&gt;</b> " + ea(cls, met) + " " + attrs.map(_.toHtml).mkString("(", ",", ")")
  override def toHtml = indent(indentLevel) + """<span class="glyphicon glyphicon-arrow-right"></span> """ + cond.map(_.toHtml+" ").mkString + ea(cls, met) + " " + toHtmlAttrs(attrs)

  override def toString = indent(indentLevel, " ") + "=> " + cond.map(_.toHtml+" ").mkString + cls + "." + met + " " + attrs.mkString("(", ",", ")")
}

object EMap {

  /** soure the attributes for a message
    *
    * @param parent message
    * @param spec specification attributes of current message (to source)
    * @param destSpec destination spec, if any
    * @param deferEvaluation
    * @param ctx
    * @return
    */
  def sourceAttrs(parent: EMsg, spec: Attrs, destSpec: Option[Attrs], deferEvaluation:Boolean=false)(implicit ctx: ECtx) = {
    // current context, msg overrides
    val myCtx = new StaticECtx(parent.attrs, Some(ctx))

    // solve an expression
    def expr(p: P) = {
      // todo if after appliedTyped we get a p with no current value but an expression, should we re-evaluate that?
      p.expr.map{x =>
        val res = x.applyTyped("")(myCtx).calculatedP
        // some exprs will return non-calculated parms (AExprIdent) - so I'm evaluating it again
        // todo should I keep calculating until I get a value?
        res
      }.getOrElse{
        // need to preserve types and stuff
        p
      }
    }

    val out1 = if (spec.nonEmpty) spec.flatMap { p =>
      if(deferEvaluation)
        List(p)
      // flattening objects first
      else if(p.expr.exists{e=>
        e.isInstanceOf[AExprIdent] &&
            e.asInstanceOf[AExprIdent].rest.size > 0 &&
            e.asInstanceOf[AExprIdent].rest.last.name.equals("asAttrs")
      }) {
        p :: flattenJson(p)
      } else if(p.name.endsWith(".asAttrs")) {
        // we have to resolve the expression here
        val ap = new SimpleExprParser().parseIdent(p.name).map(_.dropLast) // without .asAttrs
        // and flatten it
        p :: flattenJson(p.copy(expr = ap).calculatedP)
      } else {
          // do evaluation now
        // sourcing has expr, overrules
        val v =
          if(p.hasCurrentValue)
            Some(p)
            // do not recalculate expressions like diesel.msg.ea - these will produce a different value
            // important: if a parm is inherited with expression and expression is diesel.msg.ea, it would produce a
            // different value if evaluated AGAIN for the child
          else if(p.expr.nonEmpty) Some(expr(p)) // a=x
          else
          // this is why we can't override values in a message decomp
//        parent.attrs.find(_.name == p.name).orElse( // just by name: no dflt, no expr
          // ctx contains in.attrs but with overwrite
            ctx.getp(p.name)
//        )

        val tt =
          v.map{pv=>
            if(pv.ttype.nonEmpty) pv.ttype
            else {
//              if (p.ttype.isEmpty && !p.expr.exists(_.getType.nonEmpty) && (v.isInstanceOf[Int] || v.isInstanceOf[Float] || v.isInstanceOf[Long] || v.isInstanceOf[Double])) WTypes.wt.NUMBER
//              else
              if (p.ttype.isEmpty) WType(p.expr.map(_.getType).mkString)
              else p.ttype
            }
          }.getOrElse {
            WTypes.wt.UNDEFINED
          }

        List(p.copy(dflt = v.map(_.currentStringValue).mkString, ttype=tt, value = v.flatMap(_.value)))
      }
    } else if (destSpec.exists(_.nonEmpty)) destSpec.get.map { p =>
      // when defaulting to spec, order changes
      val v = /*in.attrs.find(_.name == p.name).map(_.dflt).orElse( */
      // ctx contains in.attrs but with overwrite
        ctx.getp(p.name)
            .getOrElse(
              expr(p)
            )
//      val tt = if(p.ttype.isEmpty && v.isInstanceOf[Int]) WTypes.NUMBER else ""
//      p.copy(dflt = v.toString, expr=None, ttype=tt)
      v.copy(name = p.name)
    } else {
      // if no map rules and no spec, then just copy/propagate all parms
      parent.attrs.map {a=>
        a.copy()
      }
    }

    out1.filter(_.ttype != WTypes.UNDEFINED) // undefined behave like they didn't come...
  }

  def sourcePasAttrs(in: List[PAS], deferEvaluation:Boolean=false)(implicit ctx: ECtx) = {
    // current context, msg overrides
    val myCtx = new StaticECtx(Nil, Some(ctx))

    // solve an expression
    def expr(p: P) = {
      p.expr.map(_.applyTyped("")(myCtx)/*.toString*/).getOrElse{
        // need to preserve types and stuff
        p
      }
    }

    val out1 =
    {
      // if no map rules and no spec, then just copy/propagate all parms
      in.map {a=>
        a.copy()
      }
    }

    out1
  }

}


