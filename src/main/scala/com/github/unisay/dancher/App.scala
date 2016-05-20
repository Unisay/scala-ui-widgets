package com.github.unisay.dancher

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import Actions._

object App extends JSApp {

  @JSExport
  override def main(): Unit = {
    val paragraph = Paragraph("It works!")
    val button = Button("Press me please!")
    new ActionRunner().run {
      for {
        _ ← log("Getting parent element")
        parent ← getElementById("parent")
        paragraphElement ← paragraph.create
        _ ← appendChild(parent, paragraphElement)
        buttonElement ← button.create
        _ ← appendChild(parent, buttonElement)
      } yield ()
    }
  }
}

