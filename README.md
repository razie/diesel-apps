
Diesel Apps
============

A framework for rapid service mocking, prototyping, development, testing and hosting. See it in action at [DieselApps.com](http://www.dieselapps.com).

While the main trait of this project is the use of DSLs, there are three components to DSL Hydra:

1. A light microservices and asynchronous workflow engine - can be re-used by itself, see [diesel](https://github.com/razie/diesel-wiki/tree/master/diesel)
2. A domain-driven Markdown Wiki - the basis for configuration, text-first, with support for extensible DSLs, see [wiki](WIKI.md).

Microservices, workflows and testing engine and DSL
=============================

The [diesel](https://github.com/razie/diesel-wiki/tree/master/diesel) sub-project is a stand-alone workflow and microservices mocking, prototyping and testing framework, based on akka actors and play.

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

Markdown Wiki
===========================

Embedded Markdown Wiki engine, extensible via scripting, code and DSL. It is a complete multi-tentant, SaaS-ready service and website creation and hosting environment.

Versions and technologies
========================

- scala 2.11
- Akka 2.4
- Play framework 2.4
- [commonmark](https://github.com/atlassian/commonmark-java) as the markdown parser
- mongodb/casbah/salat for persistency.
- bootstrap 3.3.4

Features
========

Here are some of the basic features:

- you can create separate project websites, like http://myproject.dieselapps.com
- you can run it on prem as well, on Docker (fork central project)
- have users, assign permissions
- supports multiple hosting right off the bat, with custom domains too

Details
==========

See the http://www.dieselapps.com/engine

See details on the [wiki](WIKI.md).
