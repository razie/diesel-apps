## Executors

These execute the messages, by "expanding" them into multiple nodes.

An executor can do something or just compute values or create sub-messages.

Derive from EExecutor and add to Executors.add().
- implement the test() method to say if you're applicable to a given message
 - test by name, or message properties etc
- implement the apply() method to do the deed

It is customary to have an executor for a "type" of messages, like `diesel.db.doc`.

Executors are synchronous or asynchronous, see EApplicable.isAsync

    * If SYNC, we'll wait in this thread on the call to apply - avoid a switch. This is great for
    * local support like DB, logging, echo and other services
    *
    * If ASYNC, then the engine will actor it out and you'll need to send a DERep to the engine when done
    * so it's more like asking the engine to isolate you rather than promising something
    *
    * In case this is async, you can return some info/correlation nodes from apply(), which will be added to the tree,
    * after which the engine will NOT mark this node complete. You will send a DERep when done.
    *
    * Also, while processing, you could add more info nodes to this one and only at the end mark it as done with DERep.
    *

