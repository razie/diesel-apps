The Diesel Wiki related components: model, controllers and views.

This module allows you to embed simple wikis, with all the basic functionality (view/edit).

Reusable bits:

### Secure links

Emailling secure links to users (reset password etc).
- Class: razie.wiki.admin.SecLink
- Controller: controllers.Sec
- Routes:
    GET            /doe/sec/$what<.*>                   @controllers.Sec.doeSec(what:String)

Sample:

    val header = request.headers.get("X-Forwarded-Host")
    val ds = SecLink("/mysecretURL/userID/etc", header, 1, DateTime.now.plusHours(1))
    sendEmail(ds.secUrl)

It will persist the secret link with a unique encoded URL and you can email that. When the user will follow the link, it will redirect to the secret link, if it has not expired (plusHours) or if the count is still below (1).

