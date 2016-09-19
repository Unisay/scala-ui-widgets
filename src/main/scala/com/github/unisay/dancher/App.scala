package com.github.unisay.dancher

import com.github.unisay.dancher.Dsl._
import com.github.unisay.dancher.Widget._
import fs2.Task.delay
import fs2.{Sink, Stream, Task}

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object App extends JSApp {

  @JSExport
  override def main(): Unit = {
    println("App started")

    case class Name(value: String) extends DomainEvent
    case class Nick(value: String) extends DomainEvent

    val handleEvents: Sink[Task, DomainEvent] =
    _.evalMap {
        case Name(name) =>
          delay(println(s"Got Name: $name!"))
        case Nick(name) =>
          delay(println(s"Got Nick: $name!"))
        case event =>
          delay(println(s"Unhandled domain event: $event"))
      }

    val root =
      body {
        verticalSplit(
          left = ask(
            title = "What is your name?",
            inputPlaceholder = "Name",
            buttonCaption = "Send Name"
          ).mapEvent { case Answer(name) => Name(name) },
          right = ask(
            title = "What is your Nickname?",
            inputPlaceholder = "Nickname",
            buttonCaption = "Send Nickname"
          ).mapEvent { case Answer(nick) => Nick(nick) }
        )
      }

    Stream
      .eval(root)
      .flatMap(_.events)
      .through(handleEvents)
      .run.unsafeRunAsyncFuture()
    ()
  }

}

