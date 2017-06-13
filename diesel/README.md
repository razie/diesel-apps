Diesel Apps - Microservices workbench
====================================

Workflows, actors and microservices.

Workflows
=========

A simple asynchronous, message-oriented workflow framework, layered on top of akka actors.

Relies on a simple DSL to define rules. The DSL has only a few constructs.

```
$when home.guest_arrived(name) => lights.on

$when home.guest_arrived(name=="Jane") => chimes.welcome(name="Jane")

$mock chimes.welcome => (greeting = "Greetings, "+name)
```

Microservices
=============

A microservices mocking, prototyping and testing framework, built with simple play framework bindings onto the diesel workflow framework: automatically turns any workflow into a microservice and/or orchestrate any microservices with the workflow rules.

Relies on a DSL to define microservices, rules and test and run these.

```
$send home.guest_arrived(name="Jane")

$expect lights.on
$expect (light is "bright")

$expect (greeting is "Greetings, Jane")
$expect chimes.welcome(name is "Janexx")
```

You can embed in your app or use as is. You can run it on-prem or in cloud, at http://www.dieselapps.com - see [The simplest micro-service you ever created](http://http://www.dieselapps.com/wiki/Cool_Scala/The_one-liner_microservice)


Versions and technologies
========================

- scala 2.11.8
- bootstrap 3.3.4
- play framework 2.4

Examples
========

Mock a simple REST API - see [The simplest microservice you ever created](http://www.dieselapps.com/wiki/Cool_Scala/The_one-liner_microservice):

```
$mock say.hi (name ?= "Jane") => (greeting = "Hello, " + name)
```

Test the simple REST API - see [Simple microservices testing](http://www.dieselapps.com/wiki/Cool_Scala/Simple_microservices_testing):

```
$send say.hi (name = "Jane")
$expect (greeting contains "Jane")
```


