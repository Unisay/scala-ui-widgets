package com.github.unisay.dancher.widget

import com.github.unisay.dancher.dom._
import Widget._

case class Label(domId: DomId, text: String) {
  def setText(textToSet: String): (Label, ActionF[DomElement]) = {
    val updatedLabel = copy(text = textToSet)
    val action = for {
      span ← updatedLabel.element
      oldChild ← span.getFirstChild
      newChild ← createTextNode(updatedLabel.text)
      _ ← span.replaceChild(newChild, oldChild)
    } yield span
    (updatedLabel, action)
  }
}

object Label extends LabelInstances {

  implicit class LabelOps(label: Label) {
    def create: ActionF[DomElement] = WidgetLabel.create(label)
  }

}

trait LabelInstances {

  implicit val WidgetLabel: Widget[Label] = new Widget[Label] {

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
