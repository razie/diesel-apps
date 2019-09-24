# DB - simple document access framework

A simple document-db access framework, based on Mongo. Basic function is
decoupling the code from Mongo specifics, so presumablty the storage can
be swapped out for other db if needed.

## Overview

The premise is using a mongo db and mapping classes/entities to collections.

Features:
- audited access: update ops are audited and logged.

See:
- `REntity` base class for entity classes - has CRUD ops
- `ROne` and `RMany` for searches (find)
- `RUpgrade` base class to the upgrade framework

Example:

```scala
/** 
  * sample entity class
  */
@RTable
case class Autosave(
  what: String,
  contents: String,
  crDtm: DateTime = DateTime.now,
  updDtm: DateTime = DateTime.now,
  _id: ObjectId = new ObjectId()) extends REntity[Autosave] {

  // override base to disble audit of these entity's ops
  override def create(implicit txn: Txn=tx.auto) = RCreate.noAudit[Autosave](this)
  override def update (implicit txn: Txn=tx.auto) = RUpdate.noAudit(Map("_id" -> _id), this)
  override def delete(implicit txn: Txn=tx.auto) = RDelete.noAudit[Autosave](this)
}

/** inventory class */
object Autosave {

  def find(what: String) = {
    ROne[Autosave]("what" -> what)
  }

  def findAll(what:String) =
    RMany[Autosave]("what" -> what)

  /** create or update */
  def set(what: String, c: String) =
    find(what)
      .map(x => x.copy(contents = c, updDtm = DateTime.now).update)
      .getOrElse(Autosave(what, c).create)

  def delete(what: String) : Unit = {
      findAll(what).toList.map(_.delete)
  }
}
```
