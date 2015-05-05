Sample razwiki usage
=====================================

To run this, just install mongo, create a database, say "razwiki" with a user and populate a single page, say Admin:Hello.

Pass the following properties on startup:

- rk.mongohost  =localhost
- rk.mongodb    =wikireactor
- rk.mongouser  =mongousername
- rk.mongopass  =mongopassword

Then point a browser to the following url: http://localhost:9000/wiki/Admin:Hello

Detailed build instructions
====================

Install mongodb

Start mongodb

Create a database


Install SCala

Install Play

Install sbt

Clone this repository


Code to look at
====================

app/Global.scala to see how to initialize the wiki engine (persistence and parser).
conf/routes to see the basic routes

