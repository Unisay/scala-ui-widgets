package com.github.unisay.dancher.widget

import com.github.unisay.dancher.Model
import com.github.unisay.dancher.dom._

case class Label(override val domId: DomId, text: String) extends Widget {

  def create = for {
    span ← createElement("span")
    _ ← span setId domId
    _ ← span setClass "d-label"
    text ← createTextNode(text)
    _ ← span appendChild text
  } yield span

  def setText(textToSet: String): (Label, ActionF[_]) = {
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

trait LabelOps {
  implicit class ModelLabelOps(model: Model) {
    def label(text: String)(implicit idGen: Generator[DomId]): Model = label(idGen.generate, text)
    def label(id: Symbol, text: String): Model = label(DomId(id.name), text)
    def label(id: DomId, text: String): Model = model.appendWidget(Label(id, text))
  }
}

object Label extends LabelOps
