/**
 *    ____    __    ____  ____  ____,,___     ____  __  __  ____
 *   (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package model

import org.bson.types.ObjectId
import org.joda.time.DateTime
import razie.audit.Audit
import razie.db._
import razie.db.tx.txn
import razie.wiki.model._

/** external user for following stuff */
@RTable
case class Follower(
  email: String,
  name: String,
  when: DateTime = DateTime.now,
  _id: ObjectId = new ObjectId()) extends REntity[Follower]

/** external user for following stuff */
@RTable
case class FollowerWiki(
  followerId: ObjectId,
  comment: String,
  uwid: UWID,
  _id: ObjectId = new ObjectId()) {

  def delete (implicit txn: Txn) = {
    Audit.delete(this);
    RDelete[FollowerWiki]("_id" -> _id)

    // remove referenced follower too, if last
    ROne[Follower](followerId)
      .filter(x=>
        RMany[FollowerWiki]("followeId" -> followerId).filter(_._id != this._id).size > 0
      )
      .map(_.delete)
  }

  def follower = ROne[Follower](followerId)
}


