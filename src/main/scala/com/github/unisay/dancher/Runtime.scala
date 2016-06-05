package com.github.unisay.dancher

import dom._

case class Runtime(initialModel: Model)(handler: DomainEventHandler) {

  val initActions = initialModel.actions.map { action ⇒
    for {
      body ← getDocumentBody
      element ← action
      _ ← body appendChild element
    } yield ()
  }

  def run(): Unit = {
    new DomCompiler().compile(initialModel, initActions) { (event, model) ⇒
      val updatedModel = handler.applyOrElse((event, model), (_: (DomainEvent, Model)) ⇒ model)
      (updatedModel, updatedModel.actions)
    }
  }

}
