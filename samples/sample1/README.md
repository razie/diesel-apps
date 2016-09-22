Sample razwiki usage
=====================================

To run this, just install scala, play and mongo, create a database with a user and populate a single page, say Admin:Hello (we'll do that on startup so it's easy).

The following properties are hardcoded:

- rk.mongohost  =localhost
- rk.mongodb    =wikireactor
- rk.mongouser  =user
- rk.mongopass  =password

Detailed build instructions
====================

Install Scala 2.11.8

Install Play 2.4

Install sbt 0.11.8

Clone this repository

Install mongodb

Start mongodb

Create a database
- use wikireactor
- db.movie.insert({"name":"tutorials point"})
- db.addUser({user:"user",pwd:"password",roles:["readWrite", "dbAdmin"]})

Then just run play in the main directory samples/sample1

Then point a browser to the following url: http://localhost:9000
