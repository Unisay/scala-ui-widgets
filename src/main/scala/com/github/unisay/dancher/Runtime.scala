package com.github.unisay.dancher

import com.github.unisay.dancher.compiler.DomCompiler
import dom._

case class Runtime(initialModel: Model)(handler: DomainEventHandler) {

  val initActions = initialModel.actions.map { action ⇒
    for {
      body ← getDocumentBody
      element ← action
      _ ← element match {
        case e: DomElement ⇒
          body appendChild e
        case _ ⇒
          noAction
      }
    } yield ()
  }

  def run(): Unit = {
    new DomCompiler().compile(initialModel, initActions) { (event, model) ⇒
      val updatedModel = handler.applyOrElse((event, model), (_: (DomainEvent, Model)) ⇒ model)
      (updatedModel, updatedModel.actions)
    }
  }

}
