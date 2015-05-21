package razie.wiki.admin

import com.mongodb.casbah.Imports._
import razie.base.data.TripleIdx
import razie.wiki.model.{UWID, WikiEntry}

import scala.collection.mutable.ListBuffer

/** something that extracts/refines info from a wiki topic or multiple wiki topics - can be cached */
object WikiRefinery {
  // todo multithread this - use Futures when re-building them
  //todo this and the observers are not dissimilar
  type REFINER = UWID => Option[WikiRefined]

  /** index entry */
  private case class WRE (var wr:Option[WikiRefined], k:String, refine:REFINER)
  private def mkWRE[T <: WikiRefined] (wr:Option[T], k:String, refine:REFINER) (implicit m : Manifest[T]) = {
    val k = m.runtimeClass.getCanonicalName
    WRE(wr, k, refine)
  }

  private val refinery = new scala.collection.mutable.HashMap[String, WRE]()
  private val cache = new TripleIdx[ObjectId, String, WRE]()
  // index dependent wiki id to dependent refined
  private val index = new scala.collection.mutable.HashMap[ObjectId, ListBuffer[WRE]]()

  def get[T <: WikiRefined] (uwid:UWID) (refine:REFINER) (implicit m : Manifest[T]) : Option[T] = {
    val k = m.runtimeClass.getCanonicalName
    if(!(refinery contains k)) {
      add(refine)
    }
    iget(uwid)
  }

  def iget[T <: WikiRefined] (uwid: UWID) (implicit m: Manifest[T]) : Option[T] = {
    val k = m.runtimeClass.getCanonicalName
    val wre = cache.get2(uwid.id, k)

    if(! wre.isDefined) {
      require(refinery contains k)
      val x = refinery.get(k)
      refinery.get(k).map {wre=>
        cache.put(uwid.id, k, new WRE(wre.refine(uwid), k, wre.refine))
      }
    }
    cache.get2(uwid.id, k).flatMap(_.wr).map(_.asInstanceOf[T])
  }

  def add[T <: WikiRefined] (refine:REFINER) (implicit m : Manifest[T]) = {
    val k = m.runtimeClass.getCanonicalName

    val wre = mkWRE(None, k, refine)
    refinery.put(k, wre)

    reindex(wre)
  }

  private def reindex (wre:WRE) = {
    wre.wr.toList.flatMap(_.uwids).foreach {uwid=>
      if(index contains uwid.id) {
        index(uwid.id).append(wre)
      } else {
        index put (uwid.id, {val x = new ListBuffer[WRE](); x.append(wre); x})
      }
    }
  }

  /** refresh and refine all dependents */
  def update (uwid:UWID) = {
    index.get(uwid.id).toList.flatMap(_.toList).toList.foreach {wre=>
      // refine the
      wre.wr = wre refine uwid

      // clean index
      index.values.foreach { lb =>
        if (lb.indexOf(wre) >= 0)
          lb.remove(lb.indexOf(wre))
      }

      reindex(wre)
    }
  }

  WikiObservers mini {
    case we:WikiEntry=> {
      // todo lazy/async
      update (we.uwid)
    }
  }
}

/** something that extracts/refines info from a wiki topic or multiple wiki topics - can be cached
  *
  * it is uniquely tied to a UWID but recalculated when dependencies change as well
  */
class WikiRefined (val uwid:UWID, val depys:Seq[UWID]) {
  def uwids = uwid +: depys
}


