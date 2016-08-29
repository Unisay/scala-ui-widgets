package com.github.unisay.dancher.interpreter

import com.github.unisay.dancher.dom.{ActionF, DomElem}

trait ActionInterpreter {

  type DomNodeT
  type DomElemT

  implicit val domElemEvidence: DomElem[DomElemT]

  def interpret[R, M](model: M, action: ActionF[R]): R

}
