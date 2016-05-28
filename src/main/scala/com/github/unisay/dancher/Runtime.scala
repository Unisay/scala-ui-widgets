package com.github.unisay.dancher

import DomAction._

case class Runtime(comparator: ModelComparator, initialModel: Model)(handler: DomainEventHandler) {

  val initAction = for {
    body ← getDocumentBody
    layout ← initialModel.create
    _ ← body appendChild layout
  } yield ()

  def run(): Unit = {
    new ModelInterpreter().interpret[DomainEvent](initialModel, initAction) { (event, model) ⇒
      val updatedModel = handler.applyOrElse((event, model), (_: (DomainEvent, Model)) ⇒ model)
      (updatedModel, comparator.diff(model, updatedModel).map(_ ⇒ ()))
    }
  }

}
