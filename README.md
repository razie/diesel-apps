Cool Scala Wiki
===============

Embedded Markdown Wiki: a wiki engine written in scala, which you can embed in any application.

Status: WIP (still refactoring it out of a bigger project, some features missing) but functional.

Features:

- markdown as the wiki langauge
- classic wiki link syntax with customizable extensions
- mongoDB for storage, customizable persistance
- customizable authentication and authorization

Versions and technologies

- scala 2.10.4
- knockoff as the markdown parser
- mongodb/casbah/salat for persistency.
- bootstrap 2.3

You can embedd this in any Java or Scala project. Here are a few websites built on top of this wiki engine:

- http://www.coolscala.com - a blog site
- http://www.racerkidz.com - a sports wiki with calendar and forms extensions, club membership etc
- https://www.nofolders.net - an online note taking application
- http://www.effectiveskiing.com - online ski teaching app

See the demos/samples included:

- [sample1 - simple text wiki](samples/sample1)

Details
==========

See the [http://www.wikireactor.com/engine]

What's a wiki domain engine? Well, more than a plain wiki engine... read some of this wiki engine's features here: http://www.racerkidz.com/wiki/Admin:Wiki_Engine - it's got quite a bit of functionality I'm exploring... including:

- [http://www.racerkidz.com/wiki/Admin:WikiPath]
- [http://blog.razie.com/2012/05/free-flowing-wiki-domain-models-and.html]

More details to come! 

Idea
=====

Many apps need editable pages and markdown is a nice, user-friendly language. Adding some wiki-style markups can make it so much more functional.

You can focus on the overall layout and functionality of your app and leave the editable details (such as description of a task or details for a calendar event or whatever) to be written in Markdown and stored as a wiki page in this embedded wiki. 


## Usage

See some usage scenarios below - more to come.

### Help pages

Use this engine to create editable help/documentation pages within your app. Users can help create/update/maintain help and documentation pages which link to each-other nicely in a web of help.

### Forums, blogs

Easily add social features embedded in your app, like forums, discussions and blogs - all as wiki topics.

### Some wiki content

The engine will store the page, render it as html (part of whatever page you will put it in) and manage links etc as per general wiki principles... making it easy for a user of your app to link from a calendar event to an action and from the action to some subjects whatnot.

### All wiki content

The other integration model, the idea that this was written for, is where your entire app is a wiki, ran by the wiki engine and you can massively customize stuff. Any entity can be modelled as a wiki page and... well, guess it.

This wiki engine is running my http//www.racerkidz.com/wiki project. Other sites powered by this wiki engine:

* http://www.enduroschool.com - my enduro blog
* http://www.askicoach.com - my ski blog
* http://www.coolscala.com - my scala and reactive stuff blog
* http://www.nofolders.net - unleash your creativy

Here's how to use the dark CSS for bootstrap: http://www.coolscala.com/wiki/Cool_Scala/Dark_skin_for_bootstrap

