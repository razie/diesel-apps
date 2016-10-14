/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.model

/** possible form states */
object FormStatus {
  final val CREATED = "created"
  final val EDITING = "editing"
  final val SUBMITTED = "submitted"
  final val APPROVED = "approved"

  final val FORM_ROLE = "formRole"
  final val FORM_STATE = "formState"
}

/** form utilities */
case class WikiForm(we: WikiEntry) {

  we.preprocessed // make sure it is parsed

  if (!we.fields.isEmpty) {
    // form design
    we.section("section", "formData").foreach { s =>
      // parse form data
      val data = razie.Snakk.jsonParsed(s.content)
      razie.MOLD(data.keys).map(_.toString).map { name =>
        val x = data.getString(name)
        we.fields.get(name) match {
          case Some(f) => we.fields.put(f.name, f.withValue(x))
          case None => we.fields.put(name, new FieldDef(name, x, Map.empty))
        }
      }
    }
  }

  def formState = we.fields.get(FormStatus.FORM_STATE).map(_.value)
  def canEdit = formState.exists(Array(FormStatus.EDITING, FormStatus.CREATED) contains _)
  def canBeApproved = formState.exists(_ == FormStatus.SUBMITTED)
  def canBeRejected = formState.exists(_ == FormStatus.APPROVED)
  def formData = we.section("section", "formData")
  def formDataJson = we.section("section", "formData").map(s => razie.Snakk.jsonParsed(s.content))
  def fields = we.fields
}

/** form utilities */
object WikiForm {

  /** try to find and parse a random formData */
  def parseFormData(c: String) = {
    val parms = new scala.collection.mutable.HashMap[String, String]()
    val PAT = """(?s)\{\{[.]?section:formData\}\}(.*)\{\{/section\}\}""".r
    PAT.findFirstMatchIn(c).foreach { m =>
      val data = razie.Snakk.jsonParsed(m.group(1))
      razie.MOLD(data.keys).map(_.toString).map { name =>
        val x = data.getString(name)
        parms.put(name, x)
      }
    }
  parms
  }
}

