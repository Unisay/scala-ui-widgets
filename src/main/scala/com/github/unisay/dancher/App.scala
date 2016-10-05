package com.github.unisay.dancher

import cats.data.Ior
import cats.instances.string._
import cats.syntax.eq._
import com.github.unisay.dancher.Dom.Event.Click
import com.github.unisay.dancher.Widget._
import com.github.unisay.dancher.widget.BasicWidgets._
import com.github.unisay.dancher.widget.LayoutWidgets._
import org.scalajs.dom.Event
import org.scalajs.dom.raw.HTMLInputElement

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object App extends JSApp {

  @JSExport
  override def main(): Unit = {
    println("App started")

    case class Model(name: String, nick: String)

    val initialModel = Model(name = "John", nick = "turk182")

    case class Name(value: String) extends DomainEvent
    case class Nick(value: String) extends DomainEvent
    case class Answer(name: String) extends DomainEvent

    def ask(title: String, inputPlaceholder: String = "", buttonCaption: String): Widget = {
      div {
        div(span(title)) :: div {
          inputText(inputPlaceholder) :: button(buttonCaption) mapTotal {
            case (ib@Binding(input: HTMLInputElement, _, _, _)) :: buttonBinding :: t =>
              ib :: buttonBinding.mapDomEventToDomain((event: Event) => event match {
                case e if e.`type` === Click.name =>
                  Ior.Both(e, Answer(input.value))
              }) :: t
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
      case (model, Name(newName)) => Effect(println(s"name = $newName")) -> model.copy(name = newName)
      case (model, Nick(newNick)) => Effect(println(s"nick = $newNick")) -> model.copy(nick = newNick)
    }

  }

}

