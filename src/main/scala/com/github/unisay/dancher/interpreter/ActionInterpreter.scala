package com.github.unisay.dancher.interpreter

import com.github.unisay.dancher.dom.{ActionF, DomElem}
import com.github.unisay.dancher.widget.Widget

trait ActionInterpreter {

  type DomNodeT
  type DomElemT
  type DomWidget[M] = Widget[DomElemT, M]

  implicit val domElemEvidence: DomElem[DomElemT]

  def interpret[R, M](model: M, action: ActionF[R]): R

}
