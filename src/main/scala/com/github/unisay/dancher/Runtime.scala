package com.github.unisay.dancher

import dom._

case class Runtime(initialModel: Model)(handler: DomainEventHandler) {

  val initAction = for {
    body ← getDocumentBody
    element ← initialModel()
    _ ← body appendChild element
  } yield ()

  def run(): Unit = {
    new ModelInterpreter().interpret(initialModel, initAction) { (event, model) ⇒
      val updatedModel = handler.applyOrElse((event, model), (_: (DomainEvent, Model)) ⇒ model)
      (updatedModel, updatedModel())
    }
  }

}
