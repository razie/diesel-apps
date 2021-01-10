package views.html.helper

import play.api.i18n.Messages
import play.api.i18n.MessagesApi
import play.api.i18n.Messages.Implicits._

import play.api.Play.current
import play.api.i18n.Messages.Implicits._

object twitterBootstrap {
  implicit val messages =
    Messages.Implicits.applicationMessagesApi(current)

}

