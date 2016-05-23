package com.github.unisay.dancher

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object App extends JSApp {

  val runtime: Runtime = new Runtime()

  case class AddLabel(event: DomMouseEvent) extends Reaction

  @JSExport
  override def main(): Unit = runtime.init {
    VerticalLayout(
      Button("Add Label", Some(AddLabel)),
      HorizontalLayout(
        Label("Horizontally"),
        Label("Placed"),
        Label("Labels")
      )
    )
  }

}

