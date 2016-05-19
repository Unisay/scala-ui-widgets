package com.github.unisay.dancher

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object App extends JSApp {

  @JSExport
  override def main(): Unit = {
    println("Started")
    val root = Label("It works!")
    new DomActionRunner().run(root.create)
  }

}

