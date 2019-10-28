# TConf - simple config framework

A light framework for decoupling configuration: decouple the management and form of configuration from its consumers.
 This allows us to have configuration contained and managed in many ways, and consumed uniformly.
 
You could have configuration in XML, property files, databases, DSL or Word documents (been there, done that, btw). 

## Overview

Any software has multiple sources of configuration, from property files 
to other, dedicated configuration files, to databases etc.

These are the `specifications` of the system, instructing it what to do...

This is a simple framework to manage the configuration and specifications:
- identify and track all configuration elements
- parse configuration
- you can uniquely reference a location in a configuration (file), 
to point out errors and where things came from.

There is a wiki implementation, in razie.wiki

Quick ref:
- `SpecPath` is a reference to a configuration element.
- `DSpec` is the base of a configuration element
- `EPos` references a position inside a config element

## Parsing

The basic parsing framework is in `parser`.

A parser will parse a `DSpec` into nodes `AstNode` which can be 
`folded` lazily, to get the result. While folding, the nodes can parse anc
collect all kinds of syntax and semantic elements, rules etc.

- `AstNode` - the basic node resulting from parsing
- `FoldingContext` - lazy parsing context

## Tags and queries

Specifications have tags and attributes.

## Inventories

Specification inventories are how you can plugin your own sources for specifications.

## Draft vs published

We support draft specs as opposed to published specs. 
