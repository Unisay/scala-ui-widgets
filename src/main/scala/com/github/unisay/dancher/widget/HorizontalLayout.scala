package com.github.unisay.dancher.widget

import com.github.unisay.dancher.{DomBinding, ModelBuilder}
import com.github.unisay.dancher.dom._

case class HorizontalLayout(domId: DomId, children: Vector[Widget] = Vector.empty) extends WidgetContainer {

  type T = HorizontalLayout

  def create =
    for {
      div ← createElement("div")
      _ ← div.setId(domId)
      _ ← div.setClass("d-horizontal-layout")
      _ ← createChildren(div)
    } yield DomBinding(node = div)

  def withChildren(children: Vector[Widget]): T = copy(children = children)
  def appendChild(widget: Widget): T = copy(children = children :+ widget)
}

trait HorizontalLayoutOps {
  implicit class ModelHorizontalLayoutOps(model: ModelBuilder) {

    def horizontal(nested: ModelBuilder ⇒ ModelBuilder)(implicit idGen: Generator[DomId]): ModelBuilder =
      horizontal(idGen.generate)(nested)

    def horizontal(id: DomId)(nested: ModelBuilder ⇒ ModelBuilder): ModelBuilder =
      model.appendWidgetContainer(HorizontalLayout(id))(nested)

  }
}

object HorizontalLayout extends HorizontalLayoutOps
