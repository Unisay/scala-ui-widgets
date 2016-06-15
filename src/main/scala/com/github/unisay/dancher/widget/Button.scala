package com.github.unisay.dancher.widget

import com.github.unisay.dancher._
import com.github.unisay.dancher.dom._

case class Button(domId: DomId, label: String, clickHandler: DomEventHandler = NoEventHandler)

object Button extends ButtonInstances {

  implicit class ButtonOps(button: Button) {
    def create: ActionF[DomElement] = WidgetButton.create(button)
  }

}

trait ButtonInstances {
  implicit val WidgetButton: Widget[Button] = new Widget[Button] {

    def domId(button: Button): DomId = button.domId

    def create(button: Button): ActionF[DomElement] =
      for {
        buttonElement ← createElement("button")
        _ ← buttonElement setId button.domId
        _ ← buttonElement setClass "d-button"
        text ← createTextNode(button.label)
        _ ← buttonElement appendChild text
        _ ← buttonElement.onClick(button.clickHandler)
      } yield buttonElement

  }
}
