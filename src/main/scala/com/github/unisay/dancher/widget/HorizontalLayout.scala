package com.github.unisay.dancher.widget

import com.github.unisay.dancher.Model
import com.github.unisay.dancher.dom._

case class HorizontalLayout(domId: DomId, children: Vector[Widget]) extends WidgetContainer {

  override def create = super.create.flatMap(_.setClass("d-horizontal-layout"))

  def removeChild(id: DomId): (HorizontalLayout, ActionF[DomNode]) =
    children.find(_.domId == id)
      .map(child ⇒ (copy(children = children.filterNot(_ == child)), child.remove))
      .getOrElse((this, node))

  def setChildren(children: Vector[Widget]): WidgetContainer = copy(children = children)
}

trait HorizontalLayoutOps {
  implicit class ModelHorizontalLayoutOps(model: Model) {
    def horizontal(f: Model ⇒ Model)(implicit idGen: Generator[DomId]): Model = horizontal(idGen.generate)(f)
    def horizontal(id: DomId)(f: Model ⇒ Model): Model = model.appendWidgetContainer(HorizontalLayout(id, _))(f)
  }
}

object HorizontalLayout extends HorizontalLayoutOps
