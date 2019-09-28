# Diesel Apps

A Scala DSL framework for developing domain and rules-driven reactive services and apps. Rapid mocking, prototyping, development, testing and hosting of (micro)services and websites. See it in action at [DieselApps.com](http://www.dieselapps.com).

Components:

1. [diesel](/diesel) - a light reactive rules-based workflow engine - can be re-used by itself
1.1. [tconf](/diesel/src/main/scala/razie/tconf) - for specs-driven logic
1.1. [dom](/diesel/src/main/scala/razie/diesel/dom) -
1. [diesel-wiki](WIKI.md) - A domain-driven Markdown Wiki - the basis for configuration, text-first, with support for extensible DSLs
1. [diesel-play](/wiki) - the play code to make everything work as a website

## Diesel - Rules and workflows

Rules, flows, actors and microservices.

A simple asynchronous, message-oriented workflow framework, driven by rules and layered on top of akka actors.

```js
$when home.guest_arrived => lights.on

$when home.guest_arrived(name=="Jane") => chimes.welcome(name)

$mock chimes.welcome(name) => (greeting = "Greetings, "+name)
```

## Testing

Relies on a DSL to define microservices, rules and test and run these.

```js
$send home.guest_arrived(name="Jane")

$expect lights.on
$expect (light is "bright")

$expect (greeting is "Greetings, Jane")
$expect chimes.welcome(name is "Janexx")
```

You can embed in your app or use as is. You can run it on-prem or in cloud, at http://www.dieselapps.com - see [The simplest micro-service you ever created](http://www.dieselapps.com/wiki/Cool_Scala/The_one-liner_microservice)

## Versions and technologies

- scala 2.11
- akka 2.4, with akka-cluster etc
- play framework 2.4

## Examples

Mock a simple REST API - see [The simplest microservice you ever created](http://www.dieselapps.com/wiki/Cool_Scala/The_one-liner_microservice):

```js
$mock say.hi (name ?= "Jane") => (greeting = "Hello, " + name)
```

Test the simple REST API - see [Simple microservices testing](http://www.dieselapps.com/wiki/Cool_Scala/Simple_microservices_testing):

```js
$send say.hi (name = "Jane")
$expect (greeting contains "Jane")
```

### Scala client - use as a rules library

```scala
implicit val system = ActorSystem("testsystem", ConfigFactory.parseString(""" """))

// tell the engine to use this system
DieselAppContext.setActorSystem(system)

// make a spec
val spec = DieselTextSpec (
    "spec_name",
    """
      |$when home.guest_arrived => lights.on
      |
      |$when home.guest_arrived(name=="Jane") => chimes.welcome(name)
      |
      |$when chimes.welcome => (greeting = "Greetings, "+name)
      |""".stripMargin
  )

  // make a story
  val story = DieselTextSpec (
    "story_name",
    """
      |$send home.guest_arrived(name="Jane")
      |""".stripMargin
  )

// run it: create engine, run story and wait for result
val engine = DomEngineUtils.execAndWait(
  DomEngineUtils.mkEngine(
    new DomEngineSettings().copy(realm=Some("rk")),
    List(spec),
    List(story)
    )
  )

println(engine.root.toString)    // debug trace of engine's execution
println(engine.resultingValue)   // resulting value, if any

// test it
assert(engine.resultingValue contains "Greetings, Jane")
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
