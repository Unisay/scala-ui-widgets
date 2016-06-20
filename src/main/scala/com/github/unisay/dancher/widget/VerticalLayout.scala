package com.github.unisay.dancher.widget

import com.github.unisay.dancher.Model
import com.github.unisay.dancher.dom._

case class VerticalLayout(domId: DomId, children: Vector[Widget]) extends WidgetContainer {

  type T = VerticalLayout

  override def create =
    for {
      div ← createElement("div")
      _ ← div.setId(domId)
      _ ← div.setClass("d-vertical-layout")
      _ ← createChildren(div)
    } yield div

  def withChildren(children: Vector[Widget]): T = copy(children = children)
}

trait VerticalLayoutOps {
  implicit class ModelVerticalLayoutOps(model: Model) {
    def vertical(f: Model ⇒ Model)(implicit idGen: Generator[DomId]): Model = vertical(idGen.generate)(f)
    def vertical(id: DomId)(f: Model ⇒ Model): Model = model.appendWidgetContainer(VerticalLayout(id, _))(f)
  }
}

object VerticalLayout extends VerticalLayoutOps
