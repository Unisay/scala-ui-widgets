package com.github.unisay.dancher

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import Action._

object App extends JSApp {

  @JSExport
  override def main(): Unit = {

    def updateHorizontalLayout(model: Widget): ActionF[DomElement] = ???

    lazy val model: Widget = VerticalLayout (
      Button("Add Label", Some(_ ⇒ updateHorizontalLayout(model))),
      HorizontalLayout (
        Label("Horizontally"),
        Label("Placed"),
        Label("Labels")
      )
    )

    new ActionRunner().run {
      for {
        body ← getDocumentBody
        layout ← model.create
        _ ← body appendChild layout
      } yield ()
    }
  }
}

