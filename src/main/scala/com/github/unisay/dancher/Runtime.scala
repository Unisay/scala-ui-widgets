package com.github.unisay.dancher

import Action._

class Runtime {

  val interpreter: ActionInterpreter = new DomInterpreter

  def init(widget: Widget): Unit = interpreter.interpret {
    for {
      body ← getDocumentBody
      layout ← widget.create
      _ ← body appendChild layout
    } yield ()
  }

}
