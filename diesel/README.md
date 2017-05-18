Diesel Apps - Microservices workbench
====================================

A microservices mocking, prototyping and testing framework.

Relies on a DSL to define microservices, rules and test and run these.

You can embed in your app or use as is. You can run it on-prem or in cloud, at http://www.dieselapps.com

Versions and technologies
========================

- scala 2.11.8
- bootstrap 3.3.4
- play framework 2.4

Examples
========

Mock a simple REST API - see (http://www.dieselapps.com/wiki/Cool_Scala/The_one-liner_microservice)[The simplest microservice you ever created]:

```
$mock say.hi (name ?= "Jane") => (greeting = "Hello, " + name)
```

Test the simple REST API - see (http://www.dieselapps.com/wiki/Cool_Scala/Simple_microservices_testing)[Simple microservices testing]:

```
$send say.hi (name = "Jane")
$expect (greeting contains "Jane")
```


