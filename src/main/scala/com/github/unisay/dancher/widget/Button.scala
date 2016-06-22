package com.github.unisay.dancher.widget

import com.github.unisay.dancher._
import com.github.unisay.dancher.dom._

case class Button(domId: DomId, label: String) extends Widget {

  def create = for {
    button ← createElement("button")
    _ ← button setId domId
    _ ← button setClass "d-button"
    text ← createTextNode(label)
    _ ← button appendChild text
    clicks ← button.clickStream
  } yield DomBinding(element = button, events = Some(clicks))

}

trait ButtonOps {
  implicit class ModelButtonOps(model: ModelBuilder) {

    def button(label: String)(implicit idGen: Generator[DomId]): ModelBuilder =
      button(idGen.generate, label)

    def button(id: Symbol, label: String): ModelBuilder =
      button(DomId(id.name), label)

    def button(id: DomId, label: String): ModelBuilder =
      model.appendWidget(Button(id, label))
  }
}

object Button extends ButtonOps
