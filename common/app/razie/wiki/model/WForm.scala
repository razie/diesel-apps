/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.model

import controllers.VErrors

/** form utils */
object WForm {
  /** format form fields as a content section */
  def formData(j: org.json.JSONObject) = {
    val co =
      "{{.section:formData}}\n" +
      j.toString +
      "\n{{/section}}\n"
    co
  }
}

/** wraper with form utils */
class WForm(val we: WikiEntry) {
  /** format form fields as a content section
    *
    * this assumes the content is just an `[[include:...]]`
    */
  def mkContent(j: org.json.JSONObject, content:String = we.content) =
  if(content.contains("{{.section:formData}}"))
    content.replaceFirst("(?s)\\{\\{.section:formData\\}\\}.*\\{\\{/section\\}\\}", "") + WForm.formData(j)
  else
    content.replaceFirst("(?s)\\]\\].*", "]]") + "\n" + WForm.formData(j)

  /** replace the fields in content with the respective html code */
  def formatFields(content: String) = {
    val FIELDS = """`\{\{\{(f):([^}]*)\}\}\}`""".r

    val ro = "" //if(we.exists(_.fields.get("formState").map(_.value).getOrElse("editing") != "editing")) "readonly" else ""

    var res = FIELDS replaceSomeIn (content, { m =>
      try {
          val name = m group 2
          val f = we.form.fields.get(name)
          val t = f.flatMap(_.attributes.get("type")).getOrElse("text")
          val other = f.map(_.attributes.filter(t =>
            t._1 != "label" &&
            t._1 != "type" &&
            t._1 != "choices"
          ).foldLeft("")((s, t) => s + " " + t._1 + "=\"" + t._2 + "\"")).getOrElse("")

          var res = f.flatMap(_.attributes.get("label")).map{l=>
            s"""  <div class="form-group">
               |<label for="$name" class="control-label">$l</label>""".stripMargin
          }.mkString

          res += {
            f.flatMap(_.attributes.get("type")) match {
              case Some("select") => {
                val choices = (f.flatMap(_.attributes.get("choices"))).toList.flatMap(_.split("\\|")).map(
                  v=>if(v.contains(";")) v.split(";") else Array(v,v)
                ).map(
                  a => "<option value=\"" +a(0) + "\" " + (if (f.get.value == a(0)) "selected>" else ">") + a(1) + "</option>").mkString
                s"""<select $ro name=$name $other autocomplete="off">$choices</select>"""
                //autocomplete off is a trick for firefox not using selected...
                // http://stackoverflow.com/questions/4831848/firefox-ignores-option-selected-selected
              }
              case _ => {
                val checked = if (f.exists(_.value == "y")) "checked" else ""
                if (t == "textarea" || t == "memo")
                  s"""
                     |<textarea $ro type="$t" name="$name" $other>
                     |${f.map(_.value).getOrElse("?")}
                     |</textarea>
                     |""".stripMargin
                else if (t == "note") {
                  // notes can be inserted as needed - with a popup
                  val v = f.map(_.value).getOrElse("")
                  val glyph = "glyphicon-list-alt"
                  s"""<div><div style="display:inline-block"><a href="#" class="btn btn-warning btn-xs" title="Edit note" onclick="return weFormEditNote('wikiForm-${we._id.toString}','$name');"><span class="glyphicon $glyph"></span></a></div>"""+
                    s"""&nbsp;<div  style="display:inline-block" name="${name}-holder">$v</div>"""+
                    s"""<input type="text" name="${name}" hidden value="$v"></input></div>"""
                } else if (t == "checkbox") {
                  s"""<input $ro type="$t" name="$name" value="y" $checked $other></input>"""
                } else if (t == "date") {
                  // <a href="#" class="btn btn-mini btn-info" title="Internet explorer likes yyyy-mm-dd format while other browsers like dd-mm-yyyy">?</a>"""
                  s"""<input $ro type="text" name="$name" value="${f.map(_.value).getOrElse("?")}" $other></input>"""
                } else
                  s"""<input $ro type="$t" name="$name" value="${f.map(_.value).getOrElse("?")}" $other></input>"""
              }
            }
          }

          res += f.flatMap(_.attributes.get("label")).map{l=>
            s"""  </div>"""
          }.mkString

          Some (res)
      } catch { case _: Throwable => Some("!?!") }
    })

    res
  }

  /** validate and format the raw field data: type, range etc */
  def validate(data: Map[String, String]) = {
    implicit val errCollector = new VErrors()
    val errors = collection.mutable.Map[String, String]()
    val newData = collection.mutable.Map[String, String]()

    data.foreach {
      case (n, iv) =>
        try {
          val v = iv.trim
          newData.put(n, v)

          // TODO this is stupid

          val SPEC = "[<>${}\\[\\]]"
          if (v.matches(".*" + SPEC + ".*")) {
            errors += n -> "Contains special characters - don't use dollar sign or (){} or <>"
            newData.put(n, v.replaceAll(SPEC, ""))
          }

          if (v.length > 2000) {
            errors += n -> "Too long - max 2000"
            newData.put(n, v.substring(0, 1999))
          }

          val f = we.form.fields.get(n)

          val t = f.flatMap(_.attributes.get("type")).getOrElse("text")
          val req = f.flatMap(_.attributes.get("req")).exists(_ == "yes")
          val other = f.map(_.attributes.filter(t => !Array("type","choices","req").contains(t._1)).foldLeft("")((s, t) => s + " " + t._1 + "=\"" + t._2 + "\"")).getOrElse("")

          if(req && v.isEmpty)
            errors += n -> "Is required! Please fill in this field."

          t match {
            case "date" => {
              if (!(v.isEmpty || v.matches("""\d\d.\d\d.\d\d\d\d""") || v.matches("""\d\d\d\d.\d\d.\d\d""")))
                errors += n -> "Not a proper date format - please use yyyy-mm-dd"
            }
            case "select" => {
              //              val choices = (f.flatMap(_.attributes.get("choices"))).toList.flatMap(_.split("\\|")).map(v => "<option " + (if (f.get.value == v) "selected>" else ">") + v + "</option>").mkString
            }
            case "number" => {
              try {
              } catch { case e: Throwable => errors += n -> s"Not a number" }
            }
            case _ => {
              //              val checked = if (f.exists(_.value == "y")) "checked" else ""
              //              if (t == "textarea" || t == "memo")
              //                s"""<textarea $ro type="$t" name="$name" $other>${f.map(_.value).getOrElse("?")}</textarea>"""
              //              else if (t == "checkbox") {
              //                s"""<input $ro type="$t" name="$name" value="y" $checked $other></input>"""
              //              } else
              //                s"""<input $ro type="$t" name="$name" value="${f.map(_.value).getOrElse("?")}" $other></input>"""
            }
          }
        } catch { case e: Throwable => errors += n -> s"ERROR: ${e.getLocalizedMessage()}" }
    }
    (newData, errors)
  }

}
