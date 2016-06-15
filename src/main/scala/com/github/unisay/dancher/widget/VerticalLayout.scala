package com.github.unisay.dancher.widget

import com.github.unisay.dancher.dom._
import cats.syntax.traverse._
import cats.std.list._

object VerticalLayout extends VerticalLayoutInstances

trait VerticalLayoutInstances {
  implicit val WidgetVerticalLayout: Widget[VerticalLayout] = new Widget[VerticalLayout] {

    def domId(w: VerticalLayout): DomId = w.domId

    def create(layout: VerticalLayout): ActionF[DomElement] = {
      for {
        div ← createElement("div")
        _ ← div setId layout.domId
        _ ← div setClass "d-vertical-layout"
        elements ← layout.children.toList.map(sw ⇒ sw.widget.create(sw.instance)).sequence
        _ ← elements.map(div.appendChild).sequence
      } yield div
    }
  }

  implicit val VerticalLayoutHasWidgets: Container[VerticalLayout] = new Container[VerticalLayout] {
    def children(parent: VerticalLayout) = parent.children
    def setChildren(parent: VerticalLayout)(newChildren: Vector[SomeWidget]) = parent.copy(children = newChildren)
  }
}

case class VerticalLayout(domId: DomId, children: Vector[SomeWidget])
