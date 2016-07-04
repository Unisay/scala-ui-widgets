package com.github.unisay.dancher.widget

import com.github.unisay.dancher._
import com.github.unisay.dancher.dom._

case class Button(domId: DomId, label: String, onClick: DomEventHandler) extends Widget {

  def create = for {
    button ← createElement("button")
    _ ← button setId domId
    _ ← button setClass "d-button"
    text ← createTextNode(label)
    _ ← button appendChild text
    clicks ← button.clickStream(onClick)
  } yield DomBinding(node = button, events = Some(clicks))

}

trait ButtonOps {
  implicit class ModelButtonOps(model: ModelBuilder) {

    def button(label: String, onClick: DomEventHandler = ClickEvent)(implicit idGen: Generator[DomId]): ModelBuilder =
      button(idGen.generate, label, onClick)

    def button(id: Symbol, label: String, onClick: DomEventHandler = ClickEvent): ModelBuilder =
      button(DomId(id.name), label, onClick)

    def button(id: DomId, label: String, onClick: DomEventHandler = ClickEvent): ModelBuilder =
      model.appendWidget(Button(id, label, onClick))
  }
}

object Button extends ButtonOps
