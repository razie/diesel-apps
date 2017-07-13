
Let's explore the differences and similarities between workflows and actors (and in general asynchronous programming techniques). Maybe we can draw some interesting conclusions.

While with Sigma Systems, I had the opportunity to design, build and/or contribute to 4 different generations of -what we called- *workflow* or *work-order* processing systems: dedicated and optimized for enterprise integration, generally targeted for telecommunication providers' integration patterns. Each generation was built upon the knowledge from the previous generations, evolving in terms of environment from C++ to Java to J2EE and from proprietary messaging to JMS with XA and then back to proprietary messaging.


## Asynchronous programming

While regular, procedural synchronous programming is very effective at writing local logic, reactive programming is a must when dealing with distributed systems (or even IO like a database or a file system), because of the necessity of decoupling of the two communicating parties and also separating failure handling, when done right.

With the advent of AJAX, NIO and Node.js, reactive programming models have taken off, recently. We took a look at modern reactive programming models *here* and *here* and the conclusion was that actors offered the best model for distributed systems, because of their message orientation and true decoupling via location transparency and passing by value.

```js
//sync
val syncResult = db.syncQuery()
alert (syncResult)

//async
db.asyncQuery {asyncResult =>
  alert(asyncResult); // hmm... how many alerts would be on screen at this point?
}
```

### Granularity

Before we start taking a look at the detailed picture, we need to first agree on what the granularity of this picture is.

While writing code, we write many instructions, call functions etc - but these are not the right level of granularity for this here article. When comparing asynchronous frameworks and especially actors and workflows, the granularity level must be somewhat higher.

Workflows are a set of *tasks* or *activities* in sequence or parallel, executed synchronously or asynchronously etc. Within each of these, there are several smaller things that happen, like function calls etc, but we look at these as one processing unit.

Similarly, for actors, the granularity is at the level of *message* - as actors natively send messages to each-other and process them.

The notion of granularity is, in my mind, a very important one. We could certainly write every instruction as a message and have an IF actor and a FOR actor, but that would not really match the granularity we assign to the notion of *message*. **A message or a task must be more than just an instruction... it must make sense to be referenced by it self**.

Having gotten that out of the way, let's do a quick refresher on... acting.

## Actors

<blockquote>
<p>
In response to a message that it receives, an actor can: make local decisions, create more actors, send more messages, and determine how to respond to the next message received. Actors may modify private state, but can only affect each other through messages (avoiding the need for any locks).
</p><small> <a href="https://en.wikipedia.org/wiki/Actor_model">wikipedia</a>
</small>
</blockquote>

In essence, an (for instance akka) actor is a local object with *private* state, whose lifecycle is managed by the actor system (create/killed/suspended/persisted etc). The actor is associated to a "mailbox": a queue of messages which are processed by the actor, *in sequence*. Each message can change the state of the object and generally results in other messages being sent to other objects (either more work requests or replies etc).

It is important that the state of the actor is only affected by the messages it responds to. You could write logic inside an actor, to send 5 different messages and await their respective responses and then send yet another aggregate message, but you should not expose regular methods that allow other objects to directly change the state encapsulated inside the actor.

The actor system takes care of things such as the actor lifecycle, addressing and routing, messaging - both local and remote, failure handling / supervision etc.

## Workflows

Similarly, at least in our definition, a workflow is an object with **local state** (as attributes and variables), has a **queue of messages** (events or messages from other components) which are processed in sequence. Processing a message will change the local state and may trigger other messages to be sent to other workflows or adapters, as the workflow/process continues and processing the notifications in sequence avoids issues like concurrent modifications of state variables. I think you're starting to see the similarity here...

The typical workflow constructs allow sending of messages in parallel or sequence and also, different workflows do not normally share any data, even though they may share the same specification/rules.

Depending on the implementation, the workflows (as in current state) may be persisted or not and the system may provide services such as transparent fail-over or not.

The workflow system will take care of things like the workflow lifecycle (persist, suspend etc), addressing/routing and finding workflows, messaging, caching, etc.

## Lambdas

...or functions... I think we need to bring this in as well, at this point. A lambda is really a bundle of related code, that... well, does something of relevance.

I'm not going to look at simple usage of lambdas in calculations, iterations or fuzzy monads, but instead, it is very interesting to note the emergence of *lambda services*... and this will tie them into the current discussion.

## VS

Actors are generally coded light-weight local objects (in terms of state), while workflows are often heavy-weight persisted and configured transactional beasts, but beyond this simple observations, what can we see?

### Visibility

The biggest difference between workflows and actors that I can see is that **an actor is a block of code** and thus can contain any weird logic, generally with blob-like invisibility, while **a workflow is generally a configured graph** of activities, likely with a graphical UI and thus it would be more restrictive as far as what you can express there, as you can generally only use specific constructs, offered by the framework you use (BPEL, BPMN, XPDL etc).

todo diag actor vs flow sample

The current state of a business process consists of the many messages waiting in different queues throughout the system and usually there is no centralized status maintained. Each actor concerns itself with only processing individual messages and not how these messages are correlated, across actors, for a greater good.

A workflow/process in fact represents the business process directly, or a part of it and the state is encoded in the flow, as the state of the different activities is updated as different events occur.

So a workflow would be more *visible* or *transparent*, in terms of how actions/tasks and sub-workflows are connected, especially when looking at the current state etc.

### Pins

In a cluster, actors are generally pinned (a reference includes the node) and managing their lifecycle (i.e. hiccups and failures) falls to the sender (as in re-sending or re-routing), while workflows tend to not be pinned... for instance, it is customary to cache workflows in memory, to speed up processing and have messages thus addressed not to the node in question

One simple way to implement transparent fail-over for instance, is to include the source node in the requests to other components and then have the components reply to the same node - if it's up. If not reachable, then direct the reply to any other node, which will then pick up the work... this scheme, augmented with some other slight changes, it will be quite efficient while still being simple to debug.

With actors, you can't quite do this automatically, since you have no insight into the logic of that particular actor.

### Resilience

While for actors, being a generic model, bulk-heading and circuit breakers and such are considerations that may or may not have to be either built or just mixed in when available, for workflow systems, these are generally built-in, in systems designed for resilience.

Whether back-pressure is implemented, in a system that allows dumping responsibility to the client or not (in a system that "must" deliver even though on a degraded basis) is a secondary concern. Allowing back-pressure to occur "naturally" via running out of sockets or other resources is not generally a good idea - common side-effects include locking out administrators exactly when they need to login the most, or kicking out users.

## Availability

Actors, being a programming model can at most be a library. Workflow and process-like cloud services however are more and more common, see solutions from MuleSoft and Azure (Logic Apps) for example, while more platforms popup every day, optimized for this or that application.

If you'd rather bungle it in-process or host it yourself, [Activiti](https://www.activiti.org/) is a good choice for the JVM (BPMN).

## Conclusion

Actor systems offer the underlying foundation to build serious reactive systems, with proper asynchronous decoupling, via message passing. They are very useful in building complex solutions to any problem.

High performance (work)flow processing systems abstract away a lot of the underlying complexity in dealing with asynchronous and reactive solutions, retaining the more interesting and useful paradigms and features, including resilience.

In a word, actors are generally the lower level construct, while the workflow is a higher level expression. So, on your next project, maybe give a thought to using a (work)flow engine or service to implement some or most of the choreography of business processes and get a lot of features for free.

If your problem is complicated enough to have to use a serious actor infrastructure, then so be it. You could however think of it as choreographed processes and implement it as you'd implement a workflow engine, with all the common resilience patterns (fail-over, location transparency, bulk-heading etc).
