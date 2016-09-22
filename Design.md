## Persistency

We use a very simple persistency model, assumed a document model, based on razie.db.RMongo

    case class MyClass (a:String,b:String, _id:ObjectId=new ObjectId()) extends REntity[MyClass]
    MyClass("a", "b").create
    ROne[MyClass]("a" -> "a")
    RMany[MyClass]("a" -> "a")
    ROne[MyClass]("a" -> "a").map(_.copy(b="c").update
    ROne[MyClass]("a" -> "a").delete
    
That's essentially it.
