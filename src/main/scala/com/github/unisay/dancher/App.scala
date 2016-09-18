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

    val handleEvents: Sink[Task, DomainEvent] =
      _.evalMap {
        case Answer(name) =>
          delay(println(s"Got Name: $name!"))
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
          ),
          right = ask(
            title = "What is your Nickname?",
            inputPlaceholder = "Nickname",
            buttonCaption = "Send Nickname"
          )
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

