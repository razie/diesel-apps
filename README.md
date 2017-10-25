
Diesel Apps
============

A framework for rapid service mocking, prototyping, development, testing and hosting. See it in action at [DieselApps.com](http://www.dieselapps.com).

While the main trait of this project is the use of DSLs, there are three components to DSL Hydra:

1. A light microservices and asynchronous workflow engine - can be re-used by itself, see [diesel](https://github.com/razie/diesel-wiki/tree/master/diesel)
2. A domain-driven Markdown Wiki - the basis for configuration, text-first, with support for extensible DSLs, see [wiki](WIKI.md).

Microservices, workflows and testing engine and DSL
=============================

The [diesel](https://github.com/razie/diesel-wiki/tree/master/diesel) sub-project is a stand-alone workflow and microservices mocking, prototyping and testing framework, based on akka actors and play.

Think of Amazon Lambda... then take all the complexity out of setting it up, allow creating the "lambdas" in real-time with a descriptive DSL, add configuration, versioning, hosting and continuous testing and deployment and you'll get a good picture. Add on-prem support and multi-vendor integration

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
