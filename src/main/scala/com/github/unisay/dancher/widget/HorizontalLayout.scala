package com.github.unisay.dancher.widget

import com.github.unisay.dancher.Model
import com.github.unisay.dancher.dom._

case class HorizontalLayout(domId: DomId, children: Vector[Widget]) extends WidgetContainer {

  type T = HorizontalLayout

  override def create =
    for {
      div ← createElement("div")
      _ ← div.setId(domId)
      _ ← div.setClass("d-horizontal-layout")
      _ ← createChildren(div)
    } yield div

  def withChildren(children: Vector[Widget]): T = copy(children = children)
}

trait HorizontalLayoutOps {
  implicit class ModelHorizontalLayoutOps(model: Model) {
    def horizontal(f: Model ⇒ Model)(implicit idGen: Generator[DomId]): Model = horizontal(idGen.generate)(f)
    def horizontal(id: DomId)(f: Model ⇒ Model): Model = model.appendWidgetContainer(HorizontalLayout(id, _))(f)
  }
}

object HorizontalLayout extends HorizontalLayoutOps
