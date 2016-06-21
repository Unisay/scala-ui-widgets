package com.github.unisay.dancher

import com.github.unisay.dancher.compiler.DomCompiler
import dom._

case class Runtime(initialModel: Model) {

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

  val compiler = new DomCompiler()

/*  def run(): Observable[ModelEvent] = {
    ???
  }*/

}
