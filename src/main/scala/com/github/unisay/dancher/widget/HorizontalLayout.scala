package com.github.unisay.dancher.widget

import com.github.unisay.dancher.{DomBinding, ModelBuilder}
import com.github.unisay.dancher.dom._

case class HorizontalLayout(domId: DomId, children: Vector[Widget]) extends WidgetContainer {

  type T = HorizontalLayout

  def create =
    for {
      div ← createElement("div")
      _ ← div.setId(domId)
      _ ← div.setClass("d-horizontal-layout")
      _ ← createChildren(div)
    } yield DomBinding(element = div)

  def withChildren(children: Vector[Widget]): T = copy(children = children)
}

trait HorizontalLayoutOps {
  implicit class ModelHorizontalLayoutOps(model: ModelBuilder) {
    def horizontal(f: ModelBuilder ⇒ ModelBuilder)(implicit idGen: Generator[DomId]): ModelBuilder = horizontal(idGen.generate)(f)
    def horizontal(id: DomId)(f: ModelBuilder ⇒ ModelBuilder): ModelBuilder = model.appendWidgetContainer(HorizontalLayout(id, _))(f)
  }
}

object HorizontalLayout extends HorizontalLayoutOps
