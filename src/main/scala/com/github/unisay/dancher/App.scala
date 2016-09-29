package com.github.unisay.dancher

import cats.data.Xor
import cats.instances.string._
import cats.syntax.eq._
import cats.syntax.xor._
import com.github.unisay.dancher.Dom.Event.Click
import com.github.unisay.dancher.Syntax.PartialToTotal
import com.github.unisay.dancher.Widget._
import com.github.unisay.dancher.widget.BasicWidgets._
import com.github.unisay.dancher.widget.LayoutWidgets._
import com.outr.scribe.Logging
import org.scalajs.dom.raw.HTMLInputElement

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
    case class Answer(name: String) extends DomainEvent

    def ask(title: String, inputPlaceholder: String = "", buttonCaption: String): Widget = {
      div {
        div(span(title)) :: div {
          inputText(inputPlaceholder) :: button(buttonCaption) mapTotal {
            case (ib@Binding(input: HTMLInputElement, _, _)) :: buttonBinding :: t =>
              val pf: PartialFunction[WidgetEvent, WidgetEvent] =
                { case Xor.Left(event) if event.`type` === Click.name => Answer(input.value).right }
              ib :: buttonBinding.mapWidgetEvent(pf.total) :: t
          }
        }
      }
    }

    val root =
      body {
        verticalSplit(
          left = ask(
            title = "What is your name?",
            inputPlaceholder = initialModel.name,
            buttonCaption = "Send Name"
          ).mapDomainEvent { case Answer(name) => Name(name) },
          right = ask(
            title = "What is your Nickname?",
            inputPlaceholder = initialModel.nick,
            buttonCaption = "Send Nickname"
          ).mapDomainEvent { case Answer(nick) => Nick(nick) }
        )
      }

    Runtime.unsafeRun(initialModel, root) {
      case (model, Name(newName)) => Effect(logger.info(s"name = $newName")) -> model.copy(name = newName)
      case (model, Nick(newNick)) => Effect(logger.info(s"nick = $newNick")) -> model.copy(nick = newNick)
    }

  }

}

