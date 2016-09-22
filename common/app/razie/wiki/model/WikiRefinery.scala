package razie.wiki.model

import com.mongodb.casbah.Imports._
import razie.base.data.TripleIdx

import scala.collection.mutable.ListBuffer

/**
  * something that extracts/refines info from a wiki topic or multiple wiki topics - and the results can be cached
  *
  * Register the refiner and it will be called when the underlying wikis change
  *
  * Yes - wikiflow as in dataflow
  */
object WikiRefinery {
  // todo multithread this - use Futures when re-building them
  // todo this and the WikiObservers are not dissimilar
  type REFINER = UWID => (Boolean, Option[WikiRefined])

  /** index entry */
  private case class WRE (var wr:Option[WikiRefined], k:String, refine:REFINER)
  private def mkWRE[T <: WikiRefined] (wr:Option[T], k:String, refine:REFINER) (implicit m : Manifest[T]) = {
    val k = m.runtimeClass.getCanonicalName
    WRE(wr, k, refine)
  }

  /** just list of factories / refineries - invoked to refine when needed */
  private val refinery = new scala.collection.mutable.HashMap[String, WRE]()

  /** cache of refined products by object id and class */
  private val cache = new TripleIdx[ObjectId, String, WRE]()

  // index dependent wiki id to dependent refined
  private val index = new scala.collection.mutable.HashMap[ObjectId, ListBuffer[WRE]]()

  /** get the (cached) result of a refiner for a specific wiki
    *
    * the refiners and their cached results are indexed by classname
    */
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
    var result : Option[WikiRefined] = None

    if(! wre.isDefined) {
      //refine it
      require(refinery contains k)
      refinery.get(k).map {wre=>
        val newf = wre.refine(uwid)
        if(newf._1) {
          val newwre = new WRE(newf._2, k, wre.refine)
          cache.put(uwid.id, k, newwre)
          reindex(newwre)
        } else {
          cache.remove2(uwid.id, k)
          result = newf._2
        }
      }
    }

    (result orElse cache.get2(uwid.id, k).flatMap(_.wr)).map(_.asInstanceOf[T])
  }

  /** add a WikiRefiner to the index - the refiners and their cached results are indexed by classname */
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
  def update (we:WikiEntry) = {
    val updated = index.get(we.uwid.id).toList.flatMap(_.toList).toList.map {wre=>
      // refine the
      wre.wr.map(_.uwid).foreach { u=>
        wre.wr = (wre refine u)._2
      }

      // clean index
      index.values.foreach { lb =>
        if (lb.indexOf(wre) >= 0)
          lb.remove(lb.indexOf(wre))
        }

      wre
    }

    // reindex later so they don't overwrite as they're recalculated
    updated.foreach {wre=>
      reindex(wre)
    }
  }

  def init() = {
    // the actual main callback
    WikiObservers mini {
      case WikiEvent(_, "WikiEntry", _, Some(x), _, _, _) =>
        update(x.asInstanceOf[WikiEntry]) // todo lazy/async
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


