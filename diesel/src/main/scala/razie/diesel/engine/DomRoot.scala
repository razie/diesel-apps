/*  ____    __    ____  ____  ____  ___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

import razie.diesel.Diesel
import razie.diesel.dom.RDOM.{P, PValue, ParmSource}
import razie.diesel.dom.{RDOM, WTypes}
import razie.diesel.engine.nodes._
import razie.diesel.expr.BExprFALSE.shorten
import razie.diesel.expr._
import razie.diesel.model.DieselMsg
import razie.hosting.Website
import razie.tconf.EPos
import scala.collection.mutable.HashMap
import scala.util.Try

/** something that has a dom root tree - engines so far */
trait DomRoot {

  def root: DomAst

  implicit def ctx: ECtx

  implicit def engine: DomEngineState

  def failedTestCount = DomEngineView.failedTestCount(root)

  def errorCount = DomEngineView.errorCount(root)

  def successTestCount = DomEngineView.successTestCount(root)

  def totalTestCount = DomEngineView.totalTestedCount(root)

  def todoTestCount = DomEngineView.todoTestCount(root)

  def progress: String = DomEngineView.failedTestCount(root) + "/" + DomEngineView.totalTestedCount(
    root) + "/" + DomEngineView.todoTestCount(root)

  def totalCount = DomEngineView.totalTestedCount(root)

  /** find a node */
  def n(id: String): DomAst = root.find(id).get

  protected def collectValues[T](f: PartialFunction[Any, T]): List[T] =
    root.collect {
      case v if (f.isDefinedAt(v.value)) => f(v.value)
    }

  /** find the direct parent of this node */
  def findParent(node: DomAst): Option[DomAst] =
    root.collect {
      case a if a.children.exists(_.id == node.id) => a
    }.headOption

  /** find the direct parent of this node that meets the condition */
  def findParentWith(node: DomAst, predicate: (DomAst => Boolean)): Option[DomAst] = {
    val p = node.parent.orElse(findParent(node))
    p.filter(predicate).orElse(p.flatMap(findParentWith(_, predicate)))
  }

  def findScope(node: DomAst): DomAst =
    findParent(node).collectFirst {
      case a if a.id == root.id => a // found root
      case a if a.value.isInstanceOf[EScope] => a
      case a if a.kind == AstKinds.RECEIVED => a // todo stories told are automatically catch boundaries ???
      case a => findScope(a)
    }.get // always something - it stops at root

  /** find the level of a node - traverse the tree from the root */
  def findLevel(node: DomAst): Int =
    root.collect2 {
      case t@(a, l) if a.id == node.id => l
    }.headOption.getOrElse {
      throw new IllegalArgumentException("Can't find level for " + node)
    }

  protected def setSmartValueInContext(
    a: DomAst,
    appendToCtx: ECtx,
    v: EVal
  ): Unit = setoSmartValueInContext(Some(a), appendToCtx, v)

  protected def setoSmartValueInContext(
    a: Option[DomAst],
    appendToCtx: ECtx,
    v: EVal
  ): Unit = {

    val aei = (new SimpleExprParser).parseIdent(v.p.name)
    if (aei.isEmpty) {
      throw new DieselExprException("Left side not qualified ID:" + v.p.name)
    }

    appendValsPas(
      a,
      v.pos,
      None,
      List(PAS(aei.get, v.p.calculatedP.currentValue)),
      appendToCtx,
      AstKinds.IGNORE
    )
  }

  def setSmartValueInContext(
    a: DomAst,
    appendToCtx: ECtx,
    p: P
  ): Unit = setoSmartValueInContext(Some(a), appendToCtx, p)

  def setoSmartValueInContext(
    a: Option[DomAst],
    appendToCtx: ECtx,
    p: P
  ) = {

    val aei = (new SimpleExprParser).parseIdent(p.name)
    if (aei.isEmpty) {
      throw new DieselExprException("Left side not qualified ID:" + p.name)
    }

    appendValsPas(
      a,
      None,
      None,
      List(PAS(aei.get, p.calculatedP.currentValue)),
      appendToCtx,
      AstKinds.IGNORE
    )
  }

  /** an assignment message - execute now */
  protected def appendValsPas(
    a: Option[DomAst],
    pos: Option[EPos],
    x: Option[EMsgPas],
    attrs: List[PAS],
    appendToCtx: ECtx,
    kind: String = AstKinds.GENERATED) = {

    implicit val ctx = appendToCtx

    /** setting one value */
    def setp(parentObject: Any, parentAccessor: String, accessor: P, v: P)(implicit ctx: ECtx): P = {
      // reset - need to recalc accessors every time
      val av = accessor.copy(value = None).calculatedTypedValue
      val vcalc = v.calculatedP

      parentObject match {
        case c: ECtx => {
          // parent is context = just set value
          // accessor must be string
          if (av.contentType != WTypes.STRING && av.contentType.length > 0)
            throw new DieselExprException(s"ctx. accessor must be string - we got: ${av.value}")
          c.put(vcalc.copy(name = av.asString))
        }
        case None => {
          throw new DieselExprException(s"Object container not found ($parentAccessor) for ${accessor}")
        }

        // parent is a map/json object
        case Some(pa: RDOM.P) if pa.isOfType(WTypes.wt.JSON) => {
          val paJ = pa.calculatedTypedValue.asJson
          val newValue = PValue(
            paJ + (av.asString -> vcalc.calculatedTypedValue.value),
            WTypes.wt.JSON
          )

          pa.value = Some(newValue)

          // todo is this relevant?
          if (paJ.isInstanceOf[HashMap[String, Any]])
            paJ.asInstanceOf[HashMap[String, Any]].put(av.asString, vcalc.calculatedTypedValue.value)

          // todo need to recurse on parent, not just hardcode ctx
          setValueInContext(a, ctx, pa)
        }

        // the only source now is "diesel", so just set as long value
        // these are handled inside the ECtx
        case Some(pa: RDOM.P) if pa.isOfType(WTypes.wt.SOURCE) => {
          setp(ctx.getScopeCtx, parentAccessor, P("", parentAccessor + "." + av.asString), v)
        }

        case Some(x) => {
          throw new DieselExprException(s"Not an Object/JSON from left-side accessor: ${x.asInstanceOf[P].ttype} - $x")
        }

      }
      vcalc
    }

    // list of pairs (left-create EVAls in tree, right - populate in curr context)
    val calc = attrs.map { pas =>

      if (pas.left.rest.isEmpty) {

        // simple expression with unqualified left side p=e

        val calcp = P(pas.left.start, "").copy(expr = Some(pas.right)).calculatedP

        if (pas.left.start == Diesel.PAYLOAD) {

          // RAZ2 OLD - remove this
          // setting in current context too

          val calcp2 = P(pas.left.start, "").copy(expr = Some(pas.right)).calculatedP
          // payload must go to first scope, regardless of enclosing rule scopes
          ctx.allToScope.foreach(_.remove(Diesel.PAYLOAD))

          // set it now in base scope
          setValueInContext(a, ctx.getScopeCtx, calcp)

          // don't propagate to current scope, it creates side effects when the parent is changed
          (List(calcp), Nil) // also return it

        } else { // not payload

          // simple expression with unqualified left side p=e
          val calc = List(calcp)
          (calc, calc)
        }
      } else {
        // more complex left expr = right value
        // a append DomAst(EInfo("xx" + pas.toString).withPos(x.pos), AstKinds.DEBUG)

        val rightP = pas.right.applyTyped("")

        val rightValue = rightP.calculatedTypedValue.value
        val parentAccessor = pas.left.dropLast
        val parentObject = parentAccessor.tryApplyTyped("")
        val last = pas.left.last.get // I know size > 1
//          a append DomAst(EInfo("parentObject: "+parentObject.mkString).withPos(x.pos), AstKinds.TRACE)
//          a append DomAst(EInfo("selector: "+pas.left.rest.head.calculatedTypedValue.toString).withPos(x.pos),
//          AstKinds.TRACE)

        // found a potential parent, recurse inside
        val res =
          if (parentObject.isDefined) {

            // each branch to return the P
            parentObject.collect {

              case pa: RDOM.P if pa.isOfType(WTypes.wt.SOURCE) => {
                val av = last.copy(value = None).calculatedTypedValue.asString
                val m = pa.calculatedTypedValue.value.asInstanceOf[ParmSource]
//                m.put(P.fromSmartTypedValue(av, rightValue))
                m.put(rightP.copy(name = av).copyFrom(rightP))
                rightP.calculatedTypedValue.asNiceString
              }

              case pa: RDOM.P if pa.isOfType(WTypes.wt.JSON) => {
                val av = last.copy(value = None).calculatedTypedValue.asString
                val m = pa.calculatedTypedValue.asJson

                if (m.isInstanceOf[HashMap[String, Any]]) {
                  m.asInstanceOf[HashMap[String, Any]].put(av, rightValue)
                  pa.dirty()
                } else {
                  a.map(
                    _ append DomAst(EError("Not mutable Map: " + parentObject.mkString).withPos(pos), AstKinds.ERROR))
                }

                // no need, since we've set in hashmap - also it may have come from an Array, not an actual P so this
                // does nothing

                rightP.calculatedTypedValue.asNiceString
              }

              case pa: RDOM.P if pa.isOfType(WTypes.wt.ARRAY) => {
                val av = last.copy(value = None).calculatedTypedValue
                val ai = av.asInt
                val list = pa.calculatedTypedValue.asArray.toArray
                list(ai) = rightP.calculatedTypedValue.value

                val newv = PValue(list.toList, WTypes.wt.ARRAY)
                pa.value = Some(newv)
                rightP.calculatedTypedValue.asNiceString
              }
            }.mkString

          } else if (pas.left.start == "ctx") {
            // todo merge and reconcile with dieselScope

            // remove any overload from all contexts until the scope
            // todo should we do this? it will change the value of local parms?
            // it's important - if someone set this parm somewhere in a parent, it won't
            // act predictably - but then that's not predictable either...
            ctx.allToScope.foreach(_.remove(pas.left.restAsP.currentStringValue))

            // this is used with variables ctx[varx] = value, where varx holds the name to set
            setp(ctx.getScopeCtx, "ctx", pas.left.restAsP, rightP)

            // todo this should recurse with the rest I think?

          } else if (pas.left.start == "dieselScope" || pas.left.start == "return") {
            // todo move to DieselCtxParmSource - just that origCtx is root there, needs some recoding

            // remove any overload from all contexts until the scope
            ctx.allToScope.foreach(_.remove(pas.left.restAsP.name))

            // scope vars are set in the closest enclosing ScopeECtx or EngCtx
            // the idea is to bypass the enclosing RuleScopeECtx
            setp(ctx.getScopeCtx, "dieselScope", pas.left.restAsP, rightP)

          } else {

            val parent = pas.left.getp(pas.left.start)
            // a append DomAst(EInfo("parent: "+parent.mkString).withPos(x.pos), AstKinds.TRACE)
            // a append DomAst(EInfo("selector: "+pas.left.rest.head.calculatedTypedValue.toString).withPos(x.pos),
            // AstKinds.TRACE)

            // if parent defined, set inside, otherwise set as long parm
            if (parent.isDefined) {
              setp(parent, pas.left.start, pas.left.restAsP, rightP)
            } else {
              setp(ctx.getScopeCtx, pas.left.start, P(pas.left.exprDot, ""), rightP)
            }
          }

        // create the assignment node
        val s = res.toString
        a.map(_ append DomAst(
          EInfo(
            pas.left.toStringCalc + " = " + shorten(s, 100),
            s
            // rightP.calculatedTypedValue.asString
          ).withPos(pos),
          AstKinds.TRACE
        ))

        (Nil, Nil)
      }
    }

    // add EVals to the tree, for each left pair

    if (AstKinds.IGNORE != kind)
      a.map(_ appendAll calc.flatMap(_._1).flatMap { p =>
        if (p.ttype == WTypes.EXCEPTION) {
          p.value.map { v =>
            val err = handleError(p, v)

            DomAst(err.withPos(pos), AstKinds.ERROR).withSpec(x) :: Nil
          } getOrElse {
            DomAst(EError(p.currentStringValue) withPos (pos), AstKinds.ERROR).withSpec(x) :: Nil
          }
        } else {
          val newa = DomAst(EVal(p) withPos (pos), kind).withSpec(x)
          newa :: Nil

          // no info needed from each val as now parm name pops up details
        }
      }
      )

    // put each right pair into current context
    // we do not propagate locally values that went up
    appendToCtx putAll calc.flatMap(_._2)

    // warn about overwriting local vars in static contexts
  }

  private def handleError (p:P, v:PValue[_]) = {
    val err = if(v.value.isInstanceOf[javax.script.ScriptException]) {
      // special handling of Script exceptions - no point showing stack trace
      EError("ScriptException: " + p.currentStringValue + v.asThrowable.getMessage)
    } else {
      new EError(p.currentStringValue, v.asThrowable)
    }

    err
  }

  /** an assignment message */
  protected def appendVals(
    a: DomAst,
    pos: Option[EPos],
    spec: Option[EMsg],
    attrs: Attrs,
    appendToCtx: ECtx,
    kind: String = AstKinds.GENERATED) = {

    attrs.foreach { p =>
      val aei = (new SimpleExprParser).parseIdent(p.name)
      if (aei.isEmpty) {
        throw new DieselExprException("Left side not qualified ID:" + p.name)
      }

      appendValsPas(
        Some(a),
        pos,
        None,
        List(PAS(aei.get, p.calculatedP.currentValue)),
        appendToCtx,
        kind
      )
    }
  }

  /** only place to set a value in context, following all conventions
    *
    * note - this will not deal with sub-objects etc, that's only for PAS
    *
    * @param a
    * @param ctx
    * @param p
    */
  private def setValueInContext(a: Option[DomAst], ctx: ECtx, p: P) {
    ctx.put(p)
  }
}

object DomRoot {

  /** setting in the closest scope ctx */
  def setValueInScopeContext(ctx: ECtx, p: P) {
    ctx.getScopeCtx.put(p)
//    ctx.put(p)
  }


}