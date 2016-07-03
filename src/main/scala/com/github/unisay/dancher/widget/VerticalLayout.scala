package com.github.unisay.dancher.widget

import com.github.unisay.dancher.{DomBinding, ModelBuilder}
import com.github.unisay.dancher.dom._

case class VerticalLayout(domId: DomId, children: Vector[Widget] = Vector.empty) extends WidgetContainer {

  type T = VerticalLayout

  def create =
    for {
      div ← createElement("div")
      _ ← div.setId(domId)
      _ ← div.setClass("d-vertical-layout")
      _ ← createChildren(div)
    } yield DomBinding(div)

  def withChildren(children: Vector[Widget]): T = copy(children = children)
  def appendChild(widget: Widget): T = copy(children = children :+ widget)
}

trait VerticalLayoutOps {
  implicit class ModelVerticalLayoutOps(val builder: ModelBuilder) {

    def vertical(nested: ModelBuilder ⇒ ModelBuilder)(implicit idGen: Generator[DomId]): ModelBuilder =
      vertical(idGen.generate)(nested)

    def vertical(id: DomId)(nested: ModelBuilder ⇒ ModelBuilder): ModelBuilder =
      builder.appendWidgetContainer(VerticalLayout(id))(nested)
  }
}

object VerticalLayout extends VerticalLayoutOps
