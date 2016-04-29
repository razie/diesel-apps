Cool Scala Wiki
===============

Embedded Markdown Wiki: a wiki engine written in scala, which you can embed in any application, especially for play framework apps. It is a great match for collaborative, content-intensive websites.

Features:

- markdown as the wiki langauge
- classic wiki link syntax with customizable extensions
- mongoDB for storage, customizable persistance
- customizable authentication and authorization

Versions and technologies

- scala 2.10.4
- knockoff and pegdown as the markdown parsers
- mongodb/casbah/salat for persistency.
- bootstrap 3
- play framework 2.3

## You can't beat wikis for content-intensive websites

... and you can't beat markdown for wikis: HTML is a publishing format; Markdown is a writing format!

You can embedd this in any Java or Scala project. Here are a few websites built on top of this wiki engine:

- https://www.nofolders.net - an online note taking application
- http://www.effectiveskiing.com - online ski teaching app
- http://www.coolscala.com and http://www.enduroschool.com - blogs
- http://www.racerkidz.com - a sports wiki with calendar and forms extensions, club membership etc

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

Many content-intensive websites need editable pages and markdown is a nice, user-friendly language. Adding some wiki-style markups can make it so much more functional. You can evolve your website in real time, engaging the audience as needed etc.

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

The other integration model, the idea that this was written for, is where your entire app is a wiki, ran by the wiki engine and you can massively customize content and page layouts.

Here's how to use the dark CSS for bootstrap: http://www.coolscala.com/wiki/Cool_Scala/Dark_skin_for_bootstrap

