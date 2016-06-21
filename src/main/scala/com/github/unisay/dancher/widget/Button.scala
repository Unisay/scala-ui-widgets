package com.github.unisay.dancher.widget

import com.github.unisay.dancher._
import com.github.unisay.dancher.dom._

trait ButtonOps {

  implicit class ModelButtonOps(model: Model) {
    def button(label: String, onClick: DomEventHandler)(implicit idGen: Generator[DomId]): Model =
      button(idGen.generate, label, onClick)
    def button(id: Symbol, label: String, clickHandler: DomEventHandler): Model =
      button(DomId(id.name), label, clickHandler)
    def button(id: DomId, label: String, clickHandler: DomEventHandler): Model =
      model.appendWidget(Button(id, label, clickHandler))
  }

}

object Button extends ButtonOps

case class Button(domId: DomId, label: String, clickHandler: DomEventHandler = NoEventHandler) extends Widget {

  def create = for {
    button ← createElement("button")
    _ ← button setId domId
    _ ← button setClass "d-button"
    text ← createTextNode(label)
    _ ← button appendChild text
    _ ← button.clickStream // TODO
  } yield button

}
