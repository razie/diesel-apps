/*   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.expr

import razie.diesel.dom.RDOM.{P, ParmSource}
import razie.diesel.dom._

/** resolving qualified identifier, including arrays, ranges, json docs etc
  *
  * @param start qualified expr a.b.c - this is used in places as such... don't replace with just a
  * @param rest the rest from the first []
  */
case class AExprIdent(val start: String, rest:List[P] = Nil) extends Expr {
  def expr = start.toString + (
      if (rest.size > 0) rest.mkString("[", "][", "]")
      else ""
      )

  // NOTE start contains last .
  def exprDot(implicit ctx: ECtx) =
    start + rest.map(_.calculatedValue).mkString(".")

  // allow ctx["name"] as well as name
  def getp(name: String)(implicit ctx: ECtx): Option[P] = {
//     if("ctx" == name)
    ctx.getp(name)
  }

  override def apply(v: Any)(implicit ctx: ECtx) = applyTyped(v).calculatedValue

  // don't blow up - used when has defaults
  def tryApplyTyped(v: Any)(implicit ctx: ECtx): Option[P] =
    getp(start).flatMap { startP =>
      rest.foldLeft(Option(startP))((a, b) => access(a, b, false))
    }
  // todo why do i make up a parm?

  override def applyTyped(v: Any)(implicit ctx: ECtx): P = {
    ctx
        .getp(exprDot) // first see if value is overwritten as a long parm
        .orElse(
          getp(start).flatMap { startP =>
            rest.foldLeft(Option(startP))((a, b) => access(a, b, true))
          }
        )
        .getOrElse(P(start, "", WTypes.wt.UNDEFINED))
  }
  // todo why do i make up a parm?

  def access(p: Option[P], accessor: P, blowUp: Boolean)(implicit ctx: ECtx): Option[P] = {
    val newpname = p.map(_.name).mkString + "["+accessor+"]"
    // accessor - need to reset value, so we keep re-calculating it in context
    val av = accessor.copy(value=None).calculatedTypedValue

    p.flatMap { p =>
      // based on the type of p
      val pv = p.calculatedTypedValue

      def throwIt: Option[P] = {
        if (blowUp)
          throw new DieselExprException(
            s"Cannot access $p of type ${pv.contentType} with ${accessor} in expression: $expr"
          )
        None
      }

      //=============== index is number

      if (av.contentType == WTypes.NUMBER) {
        val ai = av.asInt

        pv.contentType match {

          case WTypes.ARRAY => {
            val list = pv.asArray
            val index =
              if (ai >= 0)
                ai
              else  // negative - subtract from length
                list.size - ai

            if(index >= 0 && index < list.size) {
              val res = list.apply(index)
              Some(P.fromTypedValue(newpname, res))
            } else {
              None
            }
          }

          case WTypes.STRING => {
            val ps = pv.asString

            if (blowUp && ps.length < ai)
              throw new DieselExprException(s"$ai out of bounds of String: $ps")

            val res =
              if (ai >= 0)
                ps.charAt(ai)
              else  // negative - subtract from length
                ps.charAt(ps.length - ai)

            Some(P.fromTypedValue(newpname, res.toString))
          }

            // we can access json attributes that can be numbers
          case WTypes.JSON =>
            val as = av.asString
            val map = pv.asJson

            val res = map.get(as)
            res match {
              case Some(v) =>
                Some(P.fromTypedValue(newpname, v))

              case None =>
                // if (blowUp) throw new DieselExprException(s"$expr does not have field $accessor")
                // else
                None // field not found - undefined
            }

          case _ => throwIt
        }

        //================================ index is string

      } else if (av.contentType == WTypes.STRING || av.contentType.isEmpty || av.contentType == WTypes.UNKNOWN) {
        pv.contentType match {

          case WTypes.SOURCE =>
            val as = av.asString

            val res = pv.value.asInstanceOf[ParmSource].getp(as)
            res match {
              case Some(v) =>
                // should we keep the name?
                Some(v.copy(name=newpname))
//                Some(v)

              case None =>
                // if (blowUp) throw new DieselExprException(s"$expr does not have field $accessor")
                // else
                None // field not found - undefined
            }

          // =============== string functions

          case WTypes.JSON =>
            val as = av.asString
            val map = pv.asJson

            val res = map.get(as)
            res match {
              case Some(v) =>
                Some(P.fromTypedValue(newpname, v))

              case None =>
                // if (blowUp) throw new DieselExprException(s"$expr does not have field $accessor")
                // else
            None // field not found - undefined
            }

            // =============== string functions

          case WTypes.STRING | WTypes.UNKNOWN if av.asString == "length" =>
            Some(P.fromTypedValue(newpname, pv.asString.length, WTypes.wt.NUMBER))

          case WTypes.STRING | WTypes.UNKNOWN if av.asString == "lines" =>
            Some(P.fromTypedValue(newpname, pv.asString.lines.toList, WType(WTypes.ARRAY, WTypes.UNKNOWN, Some(WTypes.STRING))))

          case _ => throwIt
        }

        //================================ index is rangr

      } else if (av.contentType == WTypes.RANGE) {
        // todo support reversing, if ai < zi
        var ai = av.asRange.start
        var zi = av.asRange.end

        pv.contentType match {

          case WTypes.ARRAY => {
            val list = pv.asArray

            if (ai < 0) ai = list.size + ai - 1
            if (zi < 0) zi = list.size + zi - 1
            if (zi == scala.Int.MaxValue) zi = list.size - 1

            if (blowUp && list.size < ai)
              throw new DieselExprException(s"$ai out of bounds of List")
            if (blowUp && list.size < zi)
              throw new DieselExprException(s"$zi out of bounds of List")

            val res = list.slice(ai, zi + 1)

            Some(P.fromTypedValue(newpname, res))
          }

          case WTypes.STRING => {
            val ps = pv.asString

            if (ai < 0) ai = ps.length + ai - 1
            if (zi < 0) zi = ps.length + zi - 1
            if (zi == scala.Int.MaxValue) zi = ps.length - 1

            // todo trim string
            if (blowUp && ps.length < ai)
              throw new DieselExprException(s"$ai out of bounds of String: $ps")
            if (blowUp && ps.length < zi)
              throw new DieselExprException(s"$zi out of bounds of String: $ps")

            val res = ps.substring(ai, zi + 1)

            Some(P.fromTypedValue(newpname, res.toString))
          }

          case _ => throwIt
        }
      } else if (av.contentType == WTypes.UNDEFINED) {
        // looking for some child of undefined
        Some(p)
      } else {
        throwIt
      }
    }
  }

  def dropLast = {
    this.copy(start=this.start, rest = this.rest.dropRight(1))
  }

  def last = {
    this.rest.takeRight(1).headOption
  }

  def toStringCalc (implicit ctx:ECtx) = {
    start + rest.map(_.calculatedValue).mkString("[", "][", "]")
//    rest.foldLeft(start)((a, b) => a + "." + b.calculatedValue)
  }
}


