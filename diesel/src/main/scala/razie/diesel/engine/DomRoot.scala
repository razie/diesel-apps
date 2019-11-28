/*  ____    __    ____  ____  ____  ___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

import razie.diesel.dom.RDOM.{P, PValue}
import razie.diesel.dom.{RDOM, WTypes}
import razie.diesel.engine.nodes._
import razie.diesel.expr._
import scala.collection.mutable.HashMap

/** something that has a dom root tree - engines so far */
trait DomRoot {

  def root: DomAst
  implicit def ctx : ECtx
  implicit def engine: DomEngineState

  def failedTestCount = DomEngineView.failedTestCount(root)
  def errorCount = DomEngineView.errorCount(root)
  def successTestCount = DomEngineView.successTestCount(root)
  def totalTestCount = DomEngineView.totalTestCount(root)
  def progress:String = DomEngineView.failedTestCount(root) + "/" + DomEngineView.totalTestCount(root) + "/" + DomEngineView.todoTestCount(root)
  def totalCount = DomEngineView.totalTestCount(root)

  /** find a node */
  def n(id:String):DomAst = root.find(id).get

  /** collect generated values */
  def resultingValues() = root.collect {
    // todo see in Api.irunDom, trying to match them to the message sent in...
    case d@DomAst(EVal(p), /*AstKinds.GENERATED*/ _, _, _) /*if oattrs.isEmpty || oattrs.find(_.name == p.name).isDefined */ => (p.name, p.currentStringValue)
  }

  /** collect the last generated value OR empty string */
  def resultingValue = root.collect {
    case d@DomAst(EVal(p), /*AstKinds.GENERATED*/ _, _, _) /*if oattrs.isEmpty || oattrs.find(_.name == p.name).isDefined */ => (p.name, p.currentStringValue)
  }.lastOption.map(_._2).getOrElse("")

  protected def collectValues[T] (f: PartialFunction[Any, T]) : List[T] =
    root.collect {
      case v if(f.isDefinedAt(v.value)) => f(v.value)
    }

  protected def findParent(node: DomAst): Option[DomAst] =
    root.collect {
      case a if a.children.exists(_.id == node.id) => a
    }.headOption

  def findLevel(node: DomAst): Int =
    root.collect2 {
      case t@(a,l) if a.id == node.id => l
    }.headOption.getOrElse {
      throw new IllegalArgumentException("Can't find level for "+node)
    }

  /** an assignment message */
  protected def appendValsPas (a:DomAst, x:EMsgPas, attrs:List[PAS], appendToCtx:ECtx, kind:String=AstKinds.GENERATED) = {
    implicit val ctx = appendToCtx

    /** setting one value */
    def setp (parent:Any, accessor:P, v:P)(implicit ctx:ECtx): P = {
      // reset - need to recalc accessors every time
      val av = accessor.copy(value=None).calculatedTypedValue
      val vcalc = v.calculatedP

      parent match {
        case c : ECtx => {
          // parent is context = just set value
          // accessor must be string
          if(av.contentType != WTypes.STRING)
            throw new DieselExprException(s"ctx. accessor must be string - we got: ${av.value}")
          c.put(vcalc.copy(name=av.asString))
        }
        case None => {
          throw new DieselExprException(s"parent not found")
        }

        // parent is a map/json object
        case Some(pa:RDOM.P) if pa.ttype == WTypes.wt.JSON => {
          val paJ = pa.calculatedTypedValue.asJson
          val newValue = PValue(
            paJ + (av.asString -> vcalc.value),
            WTypes.wt.JSON
          )

          pa.value = Some(newValue)

          // todo is this relevant?
          if(paJ.isInstanceOf[HashMap[String,Any]])
            paJ.asInstanceOf[HashMap[String, Any]].put(av.asString, vcalc.value)

          // no need to set dflt, use p.currentStringValue
          // todo need to recurse on parent, not just hardcode ctx
          ctx.put(pa)
        }
      }
      vcalc
    }

    val calc = attrs.flatMap { pas =>
      if (pas.left.rest.isEmpty) {
        // classic p=e
        val calcp = P(pas.left.start, "").copy(expr = Some(pas.right)).calculatedP
        val calc = List(calcp)
        calc
      } else {
        // more complex left expr = right value
        a append DomAst(EInfo(pas.toString).withPos(x.pos), AstKinds.DEBUG)

        val p = pas.right.applyTyped("")

        val res = if (pas.left.rest.size > 1) { // [] accessors
          val vvalue = p.calculatedTypedValue.value
          val parentAccessor = pas.left.dropLast
          val parent = parentAccessor.tryApplyTyped("")
          val last = pas.left.last.get // I know size > 1
//          a append DomAst(EInfo("parent: "+parent.mkString).withPos(x.pos), AstKinds.TRACE)
//          a append DomAst(EInfo("selector: "+pas.left.rest.head.calculatedTypedValue.toString).withPos(x.pos), AstKinds.TRACE)

          parent.collect {

            case pa:RDOM.P if pa.ttype == WTypes.wt.JSON => {
              val av = last.copy(value=None).calculatedTypedValue.asString
              val m = pa.calculatedTypedValue.asJson

              if(m.isInstanceOf[HashMap[String,Any]])
                m.asInstanceOf[HashMap[String, Any]].put(av, vvalue)
              else {
                a append DomAst(EError("Not mutable Map: " + parent.mkString).withPos(x.pos), AstKinds.ERROR)
              }

              val newv = PValue(
                pa.calculatedTypedValue.asJson + (av -> vvalue),
                WTypes.wt.JSON
              )
              pa.value = Some(newv )
            }

            case pa:RDOM.P if pa.ttype == WTypes.wt.ARRAY => {
              val av = last.copy(value=None).calculatedTypedValue
              val ai = av.asInt
              val list = pa.calculatedTypedValue.asArray.toArray
              list(ai) = p.calculatedTypedValue.value

              val newv = PValue(list.toList, WTypes.wt.ARRAY)
              pa.value = Some(newv)
            }
          }
        } else if (pas.left.start == "ctx") {
          setp(ctx, pas.left.rest.head, p)
        } else {
          val parent = pas.left.getp(pas.left.start)
          // a append DomAst(EInfo("parent: "+parent.mkString).withPos(x.pos), AstKinds.TRACE)
          // a append DomAst(EInfo("selector: "+pas.left.rest.head.calculatedTypedValue.toString).withPos(x.pos), AstKinds.TRACE)
          setp(parent, pas.left.rest.head, p)
        }
        a append DomAst(EInfo(pas.left.toStringCalc + " = " + res, p.calculatedTypedValue.asString).withPos(x.pos), AstKinds.DEBUG)
        Nil
      }
    }

      a appendAll calc.flatMap{p =>
        if(p.ttype == WTypes.EXCEPTION) {
          p.value.map {v=>
            val err = handleError (p, v)

            DomAst(err.withPos(x.pos), AstKinds.ERROR).withSpec(x) :: Nil
          } getOrElse {
            DomAst(EError(p.dflt) withPos (x.pos), AstKinds.ERROR).withSpec(x) :: Nil
          }
        } else {
          val newa = DomAst(EVal(p) withPos (x.pos), kind).withSpec(x)
          newa ::
          DomAst(EInfo(p.toString, p.calculatedTypedValue.asString).withPos(x.pos), AstKinds.DEBUG) :: Nil
        }
      }

      appendToCtx putAll calc
  }

  private def handleError (p:P, v:PValue[_]) = {
    val err = if(v.value.isInstanceOf[javax.script.ScriptException]) {
      // special handling of Script exceptions - no point showing stack trace
      EError("ScriptException: " + p.dflt + v.asThrowable.getMessage)
    } else {
      new EError(p.dflt, v.asThrowable)
    }

    err
  }

  /** an assignment message */
  protected def appendVals (a:DomAst, x:EMsg, attrs:Attrs, appendToCtx:ECtx, kind:String=AstKinds.GENERATED) = {
    a appendAll attrs.map{p =>
      if(p.ttype == WTypes.EXCEPTION) {
        p.value.map {v=>
          val err = handleError (p, v)

          DomAst(err.withPos(x.pos), AstKinds.ERROR).withSpec(x)
        } getOrElse {
          DomAst(EError(p.dflt) withPos (x.pos), AstKinds.ERROR).withSpec(x)
        }
      } else {
        DomAst(EVal(p) withPos (x.pos), kind).withSpec(x)
      }
    }

    appendToCtx putAll attrs

    if(appendToCtx.isInstanceOf[StaticECtx]) {
      a appendAll attrs.flatMap { p =>
        appendToCtx.asInstanceOf[StaticECtx].check(p).toList map {err=>
          DomAst(err.withPos(x.pos), AstKinds.ERROR).withSpec(x)
        }
      }
    }
  }

}

