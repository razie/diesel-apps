
Diesel Hydra
============

There are three components to DSL Hydra:

1. A light workflow engine - asynchronous, can be reused by itself
2. Diesel RX - the microservices workbench module
3. A Markdown Wiki - the basis for configuration, text-first
3.a. Extensible DSL - we can extend the wiki with DSL

See it in action at [DieselApps.com](http://www.dieselapps.com).

Markdown Wiki
===========================

Embedded Markdown Wiki engine, extensible via scripting, code and DSL. It is a full multi-tentant, SaaS-ready wiki/website hosting environment.

Microservices and testing DSL
=============================

The [diesel](https://github.com/razie/diesel-wiki/tree/master/diesel) sub-project is a stand-alone workflow and microservices mocking, prototyping and testing framework.

Versions and technologies
========================

- scala 2.11.8
- [commonmark](https://github.com/atlassian/commonmark-java) as the markdown parser
- mongodb/casbah/salat for persistency.
- bootstrap 3.3.4
- play framework 2.4

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

