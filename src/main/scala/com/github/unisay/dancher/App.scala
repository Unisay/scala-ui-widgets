package com.github.unisay.dancher

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import Action._

object App extends JSApp {

  @JSExport
  override def main(): Unit = {
    val paragraph = Paragraph("It works!")
    val button1 = Button("Press me please!", Some(_ ⇒ println("Button 1 clicked!")))
    val button2 = Button("Press me too!", Some(_ ⇒ println("Button 2 clicked!")))

    new ActionRunner().run {
      for {
        body ← getDocumentBody
        p  ← paragraph.create
        _  ← body appendChild p
        b1 ← button1.create
        _  ← body appendChild b1
        b2 ← button2.create
        _  ← body appendChild b2
      } yield ()
    }
  }
}

