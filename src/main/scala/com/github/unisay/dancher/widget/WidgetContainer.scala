package com.github.unisay.dancher.widget

import cats.std.list._
import cats.syntax.traverse._
import com.github.unisay.dancher.dom._
import monocle.Lens
import monocle.macros.GenPrism

object WidgetContainer {
  private val get: (WidgetContainer) ⇒ Vector[Widget] = _.children
  private val set: (Vector[Widget]) ⇒ (WidgetContainer) ⇒ WidgetContainer = children ⇒ _.setChildren(children)
  val _childrenLens: Lens[WidgetContainer, Vector[Widget]] = Lens(get)(set)
  val _containerPrism = GenPrism[Widget, WidgetContainer]
  val _children = _containerPrism.composeLens(_childrenLens)
}

trait WidgetContainer extends Widget {

  def children: Vector[Widget]

  def setChildren(children: Vector[Widget]): WidgetContainer

  def create = for { // TODO remember created element by returning a copy of the widget
    div ← createElement("div")
    _ ← div setId domId
    elements ← children.toList.map(_.create).sequence
    _ ← elements.map(div.appendChild).sequence
  } yield div

}




