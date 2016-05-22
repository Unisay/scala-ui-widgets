package com.github.unisay.dancher

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import Action._

object App extends JSApp {

  @JSExport
  override def main(): Unit = {

    val widget = VerticalLayout (
      Paragraph("It works!"), Button("test"),
      HorizontalLayout (
        Button("Press me please!", Some(_ ⇒ println("Button 1 clicked!"))),
        Button("Press me too!", Some(_ ⇒ println("Button 2 clicked!")))
      )
    )

    new ActionRunner().run {
      for {
        body ← getDocumentBody
        layout ← widget.create
        _ ← body appendChild layout
      } yield ()
    }
  }
}

