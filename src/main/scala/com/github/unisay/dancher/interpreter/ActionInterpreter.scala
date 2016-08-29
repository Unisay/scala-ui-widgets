package com.github.unisay.dancher.interpreter

import com.github.unisay.dancher.dom.{ActionF, DomElem, DomEvent}

trait ActionInterpreter {

  type DomNodeT
  type DomElemT
  type DomEventT

  implicit val domElemEvidence: DomElem[DomElemT]
  implicit val domEventEvidence: DomEvent[DomEventT]

  def interpret[R, M](model: M, action: ActionF[R]): R

}
