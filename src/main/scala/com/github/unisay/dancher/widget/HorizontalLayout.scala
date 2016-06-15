package com.github.unisay.dancher.widget

import com.github.unisay.dancher.dom._
import cats.syntax.traverse._
import cats.std.list._

object HorizontalLayout extends HorizontalLayoutInstances

trait HorizontalLayoutInstances {
  implicit val WidgetHorizontalLayout: Widget[HorizontalLayout] = new Widget[HorizontalLayout] {

    def domId(w: HorizontalLayout): DomId = w.domId

    def create(layout: HorizontalLayout): ActionF[DomElement] = {
      for {
        div ← createElement("div")
        _ ← div setId layout.domId
        _ ← div setClass "d-horizontal-layout"
        elements ← layout.children.toList.map(sw ⇒ sw.widget.create(sw.instance)).sequence
        _ ← elements.map(div.appendChild).sequence
      } yield div
    }
  }

  implicit val HorizontalLayoutHasWidgets: Container[HorizontalLayout] = new Container[HorizontalLayout] {
    def children(parent: HorizontalLayout) = parent.children
    def setChildren(parent: HorizontalLayout)(newChildren: Vector[SomeWidget]) = parent.copy(children = newChildren)
  }
}

case class HorizontalLayout(domId: DomId, children: Vector[SomeWidget])
