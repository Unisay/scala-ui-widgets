package com.github.unisay.dancher

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import Action._

object App extends JSApp {

  implicit val interpreter: ActionInterpreter = new DomInterpreter

  def updateHorizontalLayout(model: Widget): ActionF[DomElement] = ???

  lazy val model: Widget = VerticalLayout(
    Button("Add Label", Some(_ ⇒ updateHorizontalLayout(model))),
    HorizontalLayout(
      Label("Horizontally"),
      Label("Placed"),
      Label("Labels")
    )
  )

  @JSExport
  override def main(): Unit = {
    val init = for {
      body ← getDocumentBody
      layout ← model.create
      _ ← body appendChild layout
    } yield ()
    init.interpret
  }

}

