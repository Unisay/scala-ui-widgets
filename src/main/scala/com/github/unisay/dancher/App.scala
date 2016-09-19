package com.github.unisay.dancher

import com.github.unisay.dancher.Dsl._
import com.github.unisay.dancher.Widget._
import com.outr.scribe.Logging

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object App extends JSApp with Logging {

  @JSExport
  override def main(): Unit = {
    logger.info("App started")

    case class Model(name: String, nick: String)

    val initialModel = Model(name = "John", nick = "turk182")

    case class Name(value: String) extends DomainEvent
    case class Nick(value: String) extends DomainEvent

    val root =
      body {
        verticalSplit(
          left = ask(
            title = "What is your name?",
            inputPlaceholder = initialModel.name,
            buttonCaption = "Send Name"
          ).mapEvent { case Answer(name) => Name(name) },
          right = ask(
            title = "What is your Nickname?",
            inputPlaceholder = initialModel.nick,
            buttonCaption = "Send Nickname"
          ).mapEvent { case Answer(nick) => Nick(nick) }
        )
      }

    Runtime.unsafeRun(initialModel, root) {
      case (model, Name(newName)) => Effect(logger.info(s"name = $newName")) -> model.copy(name = newName)
      case (model, Nick(newNick)) => Effect(logger.info(s"nick = $newNick")) -> model.copy(nick = newNick)
    }

  }

}

