/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package controllers

import play.api.data.validation.{Constraint, Invalid, Valid, ValidationError}
import razie.Logging

/** common validation utilities - utilities and constants for errors/corrections and so on. mix into your controllers to get these
  *
  * this is meant to be mixed into controllers
  * */
trait PlayValidation extends Logging {

  //============= FORM validations

//  def vBadWords: Constraint[String] = Constraint[String]("constraint.badwords") { o =>
//    if (Wikis.hasBadWords(o))
//      Invalid(ValidationError("Failed obscenity filter, eh?")) else Valid
//  }

  def vPostalCode: Constraint[String] = Constraint[String]("constraint.postalCode") { o =>
    if (o.length > 0 && !o.toUpperCase.matches("[A-Z]\\d[A-Z] \\d[A-Z]\\d"))
      Invalid(ValidationError("postalCode should be like A1A 2B2, eh?")) else Valid
  }

  def vSpec: Constraint[String] = Constraint[String]("constraint.specialChars") { o =>
    if (o.contains('<') || o.contains('>'))
      Invalid(ValidationError("specialChars not allowed, eh?")) else Valid
  }

  def vEmail: Constraint[String] = Constraint[String]("constraint.emailFormat") { o =>
    if (o.trim.length > 0 && !o.trim.matches("[^@]+@[^@]+\\.[^@]+") || o.contains(" ") || o.contains("\t"))
      Invalid(ValidationError("invalid email format"))
    else if (o.contains(" ") || o.contains("\t"))
      Invalid(ValidationError("invalid spaces in email"))
    else Valid
  }

  def vldSpec(s: String) = !(s.contains('<') || s.contains('>'))
  def vldEmail(s: String) = s.matches("[^@]+@[^@]+\\.[^@]+") && !s.contains(" ") && !s.contains("\t")
}


