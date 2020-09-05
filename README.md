![DIESEL](https://cdn.razie.com/Public/diesel/diesel3round.png)

[<img src="https://img.shields.io/maven-central/v/com.razie/diesel_2.11.svg?label=latest%20release%20for%202.11"/>](http://search.maven.org/#search%7Cga%7C1%7Cg%3Acom.razie%20a%3Adiesel_2.11) [![License](https://img.shields.io/github/license/Netflix/conductor.svg)](http://www.apache.org/licenses/LICENSE-2.0)

## Reactive rules and workflow DSL

An asynchronous, message-oriented workflow framework, driven by rules and layered on top of akka actors. The rules are based on pattern matching:

```js
$when math.fact (n == 0)
=> (payload=1)

$when math.fact (n > 0)
=> math.fact (n=n-1)
=> (payload=payload * n)
```

See more details here:
- [Language reference](http://specs.dieselapps.com/Topic/DSL_Reference)
- [Expressions and pattern matching](http://specs.dieselapps.com/Topic/Expressions_and_pattern_matching)
- Towers of Hanoi example: [spec](http://specs.dieselapps.com/wiki/Spec:hanoi-spec) and [story](http://specs.dieselapps.com/wiki/Story:hanoi-story)

## Testing

The testing framework relies on similar DSL constructs, to define tests and then a [Guardian](http://specs.dieselapps.com/Topic/Guardian), to run these continuously.

```js
$send math.fact (n=0)
$expect (payload == 1)

$send math.fact (n=5)
$expect (payload == 120)
```

See more details and technical notes at [diesel](/diesel).

## Expressions

The expressions used in Diese are useful on their own: as an external DSL, the expressions are fairly complex (see more in [expr](/diesel/src/main/scala/razie/diesel/expr)), including lambdas, list operators and inlined Javascript expressions, such as:

```js
// array/lists with lambdas etc
[1,2] + [3] filter (x=> x > 1) map (x=> x + 1)

// embedded JS, when you run out of constructs
js:{var d = new Date(); d.setSeconds(d.getSeconds() + 10); d.toISOString();}
```

See [expr](/diesel/src/main/scala/razie/diesel/expr) for details and info on using them on their own.


### Execution traces

The "execution trace", a tree-like model is stored and can be retrieved to see what is going on, or what happened for a past request. This will represent, at all times, the current state of execution (some nodes may be async operations that we're waiting for etc).

![alt diesel tree](http://cdn.razie.com/Public/diesel/lights-chimes-wrong.png)

# Diesel Apps

Around the main Diesel DSL for reactive rules, we created an entire Scala DSL framework for developing domain and rules-driven reactive services and apps. Rapid mocking, prototyping, development, testing and hosting of (micro)services and websites, see [The simplest micro-service you ever created](http://www.dieselapps.com/wiki/Cool_Scala/The_one-liner_microservice)

    See it in action and go serverless at [DieselApps.com](http://www.dieselapps.com).

You can either use the [DieselApps](http://www.dieselapps.com) cloud, embed the rules or the entire framework in your app or [run your own instances on-prem](http://specs.dieselapps.com/Topic/Running_locally_via_Docker),

Components:

1. [diesel](/diesel) - the light reactive rules-based workflow engine
   * [expr](/diesel/src/main/scala/razie/diesel/expr) - expressions and parsing
   * [tconf](/diesel/src/main/scala/razie/tconf) - TBD, for specs-driven logic
   * [dom](/diesel/src/main/scala/razie/diesel/dom) - TBD, domain entities
   * [db](/diesel/src/main/scala/razie/db) - simple entity persistence layer for Mongo
   * [diesel-snakk](http://specs.dieselapps.com/wiki/Spec:rest_spec) - snakked on steroids: simple REST snakking, XML and JSON template parsing etc
   * [diesel-rest](http://specs.dieselapps.com/wiki/Spec:restMock-spec) - mocking of REST services
1. [diesel-wiki](/wiki) - A domain-driven Markdown Wiki - the basis for configuration, text-first, with support for extensible DSLs
1. [diesel-play](/wiki/app) - the play code to make everything work as a website

Head over to the [academy](http://specs.dieselapps.com/wiki/Diesel_Academy) to read more!

## Diesel-wiki

Domain driven markdown wiki. See more at [Markup_and_DSL](http://specs.dieselapps.com/Topic/Markup_and_DSL).

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

OR via the implicit REST API:

snakk.text (url = "http://specs.dieselapps.com/diesel/mock/say.hi?name=Jane")
$expect (payload contains "Jane")
```

The implicit REST API is implemented in Play Framework controllers, which you can include in your routes, see [Routes](ROUTES.md).

### Scala client - use as a rules library

You can use this library directly in your code, in several ways, see some [samples](/diesel/src/main/scala/razie/diesel/samples) or [unit tests](/diesel/src/test/scala/tests/TestSimpleEngine).

```scala
implicit val system = ActorSystem("testsystem", ConfigFactory.parseString(""" """))

// tell the engine to use this system
// tell the engine to use this system
DieselAppContext
    .withSimpleMode()
    .withActorSystem(system)

// make a DSL spec - the rules we will run
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

  // make a story DSL - the starting sequence of events
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
