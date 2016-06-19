package com.github.unisay.dancher.widget

import com.github.unisay.dancher.Model
import com.github.unisay.dancher.dom._

case class VerticalLayout(domId: DomId, children: Vector[Widget]) extends WidgetContainer {

  override def create = super.create.flatMap(_.setClass("d-vertical-layout"))

  def removeChild(id: DomId): (VerticalLayout, ActionF[DomNode]) =
    children.find(_.domId == id)
      .map(child ⇒ (copy(children = children.filterNot(_ == child)), child.remove))
      .getOrElse((this, node))

  def setChildren(children: Vector[Widget]): WidgetContainer = copy(children = children)
}

trait VerticalLayoutOps {
  implicit class ModelVerticalLayoutOps(model: Model) {
    def vertical(f: Model ⇒ Model)(implicit idGen: Generator[DomId]): Model = vertical(idGen.generate)(f)
    def vertical(id: DomId)(f: Model ⇒ Model): Model = model.appendWidgetContainer(VerticalLayout(id, _))(f)
  }
}

object VerticalLayout extends VerticalLayoutOps
