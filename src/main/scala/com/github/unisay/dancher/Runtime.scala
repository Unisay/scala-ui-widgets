package com.github.unisay.dancher

import com.github.unisay.dancher.interpreter.DomInterpreter

case class Runtime(initialModel: ModelBuilder) {

/*  val initAction =
    for {
      body ← getDocumentBody
      binding ← initialModel.action
      _ ← binding.element match {
        case e: DomElement ⇒
          body appendChild e
        case _ ⇒
          noAction
      }
    } yield binding*/


  val compiler = new DomInterpreter()

/*  def run(): Observable[ModelEvent] = {
    ???
  }*/

}
