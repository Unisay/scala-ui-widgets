package com.github.unisay.dancher.widget

import com.github.unisay.dancher.{DomBinding, ModelBuilder}
import com.github.unisay.dancher.dom._

case class VerticalLayout(domId: DomId, children: Vector[Widget]) extends WidgetContainer {

  type T = VerticalLayout

  def create =
    for {
      div ← createElement("div")
      _ ← div.setId(domId)
      _ ← div.setClass("d-vertical-layout")
      _ ← createChildren(div)
    } yield DomBinding(div)

  def withChildren(children: Vector[Widget]): T = copy(children = children)
}

trait VerticalLayoutOps {
  implicit class ModelVerticalLayoutOps(model: ModelBuilder) {
    def vertical(f: ModelBuilder ⇒ ModelBuilder)(implicit idGen: Generator[DomId]): ModelBuilder = vertical(idGen.generate)(f)
    def vertical(id: DomId)(f: ModelBuilder ⇒ ModelBuilder): ModelBuilder = model.appendWidgetContainer(VerticalLayout(id, _))(f)
  }
}

object VerticalLayout extends VerticalLayoutOps
