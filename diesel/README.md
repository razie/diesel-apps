# Diesel Apps - Microservices workbench

Rules, workflows, actors and microservices.

Think of Amazon Lambda... then take all the complexity out of setting it up, allow creating the "lambdas" in real-time with a descriptive DSL, add configuration, versioning, hosting and continuous testing and deployment and you'll get a good picture. Add on-prem support and multi-vendor integration

## Rules and workflows

A simple asynchronous, message-oriented workflow framework, layered on top of akka actors.

This is a generic rules-based engine which can be used in a variety of scenarios, not just workflow processing, but also like a "message broker" or "decomposition" or other. Relies on a simple DSL to define rules and an engine to interpret them. The DSL has only a few constructs.

```
$when home.guest_arrived(name) => lights.on

$when home.guest_arrived(name=="Jane") => chimes.welcome(name="Jane")

$mock chimes.welcome => (greeting = "Greetings, "+name)
```

## Microservices

A microservices mocking, prototyping and testing framework, built with simple play framework bindings onto the diesel workflow framework: automatically turns any workflow into a microservice and/or orchestrate any microservices with the workflow rules.

Relies on a DSL to define microservices, rules and test and run these.

```
$send home.guest_arrived(name="Jane")

$expect lights.on
$expect (light is "bright")

$expect (greeting is "Greetings, Jane")
$expect chimes.welcome(name is "Janexx")
```

You can embed in your app or use as is. You can run it on-prem or in cloud, at http://www.dieselapps.com - see [The simplest micro-service you ever created](http://www.dieselapps.com/wiki/Cool_Scala/The_one-liner_microservice)

## Engine features

### Trace model

The engine uses an innovative tree-like model, which is the "execution trace". This will represent, at all times, the current state of execution (some nodes may be async operations that we're waiting for etc).

The parallel/sequence and other complicated execution models that are available, are hidden - available only via API.

TODO tree pic

### Intuitive sync/async model

The engine uses an "intuitive" sync/async model for processing the "nodes": simple transformation nodes are executed synchronously, while other nodes (messages) are executed asynchronously.

Typical sync nodes are:
- mocks
- rules
- tests
- transformations

The intuitive part comes in when executing sequences of nodes - these are also executed sync/async **but also in sequence**, implicitly using the ask pattern and Futures.

The engine itself is always async, so you can use this model to quickly wrap synchronous operations as asynchronous processes.

### Custom execution strategies

You can easily define your own nodes and/or sync/async executors for certain messages.

## Bullet-proof

There are several hooks provided, so you could weave in your own resilience. The supported versions, at [dieselapps.com](http://www.dieselapps.com) offers bullet-proofness with any paid plan.

## Versions and technologies

- scala 2.11
- akka 2.4, with akka-cluster etc
- play framework 2.4

## Examples

Mock a simple REST API - see [The simplest microservice you ever created](http://www.dieselapps.com/wiki/Cool_Scala/The_one-liner_microservice):

```
$mock say.hi (name ?= "Jane") => (greeting = "Hello, " + name)
```

Test the simple REST API - see [Simple microservices testing](http://www.dieselapps.com/wiki/Cool_Scala/Simple_microservices_testing):

```
$send say.hi (name = "Jane")
$expect (greeting contains "Jane")
```

### Scala client - use as a rules library

```scala
val rules = List(
  TextSpec ( "spec",
"""
$when chimes.welcome => (greeting = "Greetings, "+name)
""".stripMargin
  )
)

val events = List(
  TextSpec ( "story",
"""
$msg chimes.welcome(name="Jane")
""".stripMargin
  )
)

val result = Utils.execAndWait(Utils.mkEngine(new DomEngineSettings(), rules, events)))
```
