package com.github.unisay.dancher

import com.github.unisay.dancher.compiler.DomCompiler

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


  val compiler = new DomCompiler()

/*  def run(): Observable[ModelEvent] = {
    ???
  }*/

}
