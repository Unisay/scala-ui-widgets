package com.github.unisay.dancher

import com.github.unisay.dancher.interpreter.DomInterpreter

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object TestApp extends JSApp {

  val interpreter = new DomInterpreter()

  @JSExport
  override def main(): Unit = {
    println("TestApp started")

    ()
  }

}
