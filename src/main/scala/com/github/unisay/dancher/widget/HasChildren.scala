package com.github.unisay.dancher.widget

import com.github.unisay.dancher.Widget
import com.github.unisay.dancher.dom.{ActionF, DomElement}
import monocle.macros.{GenLens, GenPrism}
import simulacrum._

@typeclass trait HasChildren[T] {

  def children[W : Widget]: Seq[W]

  def _children[W : Widget](t: T) =
    GenPrism[W, t.type] composeLens GenLens[t.type]((t: T) ⇒ implicitly[HasChildren[t.type]].children)

  def createChildren[W : Widget](children: Seq[W]): ActionF[Seq[DomElement]] = {
    /*
    override def create = super.create.flatMap(_.setClass("d-horizontal-layout"))
    def removeChild(id: DomId): WidgetAction[DomNode] =
    children.find(_.domId == id)
      .map(child ⇒ (copy(children = children.filterNot(_ == child)), child.remove))
      .getOrElse((this, node))

     */
    ???
  }
}

object HasChildren {

  implicit def VerticalLayoutHasChildren[W : Widget] = new HasChildren[VerticalLayout[W]] {
    def children[WW: Widget]: Seq[WW] = ???
  }

  implicit def HorizontalLayoutHasChildren[W : Widget] = new HasChildren[HorizontalLayout[W]] {
    def children[WW: Widget]: Seq[WW] = ???
  }
}
