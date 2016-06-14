package com.github.unisay.dancher.widget

import com.github.unisay.dancher.Widget
import com.github.unisay.dancher.Widget.WidgetAction
import com.github.unisay.dancher.dom.{DomElement, DomId, _}

object Label {

  implicit val WidgetLabel = new Widget[Label] {

    def domId(label: Label): DomId = label.domId

    def create(label: Label): ActionF[DomElement] =
      for {
        span ← createElement("span")
        _ ← span setId label.domId
        _ ← span setClass "d-label"
        text ← createTextNode(label.text)
        _ ← span appendChild text
      } yield span
  }

}

case class Label(domId: DomId, text: String) {

  def setText(textToSet: String): WidgetAction[DomElement] = {
    val updatedLabel = copy(text = textToSet)
    val action = for {
      span ← element
      oldChild ← span.getFirstChild
      newChild ← createTextNode(textToSet)
      _ ← span.replaceChild(newChild, oldChild)
    } yield span
    (updatedLabel, action)
  }

}
