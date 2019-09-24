# Diesel Apps - Microservices workbench

Rules, flows, actors and microservices.

Add on-prem support and multi-vendor integration

## Rules and workflows

A simple asynchronous, message-oriented workflow framework, driven by rules and layered on top of akka actors.

This is a generic rules-based engine which can be used in a variety of scenarios, not just workflow processing, 
but also like a "message broker" or "decomposition" or other. Relies on a simple DSL to define rules and an engine to 
interpret them. The DSL has only a few constructs.

```
$when home.guest_arrived(name) => lights.on

$when home.guest_arrived(name=="Jane") => chimes.welcome(name="Jane")

$mock chimes.welcome => (greeting = "Greetings, "+name)
```

## Microservices

These messages are bound natively and automatically to REST, resulting A microservices mocking, prototyping and 
testing framework, built with simple play framework bindings.

The diesel workflow framework: automatically turns any workflow into a microservice and/or orchestrate any microservices with the workflow rules.

## Testing

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

The engine uses a tree-like model, which is the "execution trace". This will represent, at all times, the current state of execution (some nodes may be async operations that we're waiting for etc).

![alt diesel tree](http://cdn.razie.com/Public/diesel/lights-chimes-wrong.png)

### Intuitive sync/async model

The engine uses an "intuitive" sync/async model for processing the "nodes": simple transformation nodes are executed synchronously, while all messages are executed asynchronously.

The intuitive part comes in when executing sequences of nodes - these are also executed sync/async **but in sequence**, 
implicitly using the ask pattern and Futures, very much like a sequence of messages processed by the same actor. In fact,
 each workflow has an associated akka actor which will execute the messages in sequence, but without blocking threads etc.

The engine itself is always async, so you can use this model to quickly wrap synchronous operations as asynchronous processes.

There are special constructs, for when you need to allow truly parallel execution of messages.

### Custom execution strategies

You can easily define your own nodes and/or sync/async executors for certain messages.


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
