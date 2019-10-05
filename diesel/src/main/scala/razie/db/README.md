# DB - simple document access framework

A simple document-db access framework, based on Mongo. Basic function is
decoupling the code from Mongo specifics, so presumablty the storage can
be swapped out for other db if needed.

## Overview

The premise is using a mongo db and mapping classes/entities to collections.

Features:
- audited access: update ops are audited and logged, good as a general principle.

See:
- `RTable` annotate case classes with this - optionally change collection name
- `REntity` base class for entity classes - has CRUD ops
 - or `REntityNoAudit` - for entities where you dno't want to audit operations (like Audit itself or irrelevant events etc)
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
  _id: ObjectId = new ObjectId()) extends REntityNoAudit[Autosave] {
}

/** inventory class */
object Autosave {
  implicit txn: Txn=tx.auto

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
