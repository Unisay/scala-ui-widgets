package com.github.unisay.dancher.widget

import cats.std.vector._
import cats.syntax.traverse._
import com.github.unisay.dancher.dom._
import monocle.macros.GenPrism
import monocle.{Lens, Optional}

object WidgetContainer {
  val _containerPrism = GenPrism[Widget, WidgetContainer]
  private val get: (WidgetContainer) ⇒ Vector[Widget] = _.children
  private val set: (Vector[Widget]) ⇒ (WidgetContainer) ⇒ WidgetContainer = children ⇒ _.withChildren(children)
  val _childrenLens: Lens[WidgetContainer, Vector[Widget]] = Lens(get)(set)
  val _children: Optional[Widget, Vector[Widget]] = _containerPrism.composeLens(_childrenLens)
}

trait WidgetContainer extends Widget {


  type T <: WidgetContainer

  def children: Vector[Widget]

  def withChildren(children: Vector[Widget]): T

  def appendChild(widget: Widget): WidgetContainer = ???

  def createChildren(parent: DomElement): ActionF[Vector[DomNode]] =
    children.map(_.create).sequence.flatMap(_.map(binding ⇒ parent.appendChild(binding.node)).sequence)

  def removeChild(id: DomId): Option[(T, ActionF[DomNode])] =
    children.find(_.domId == id).map(child ⇒ (withChildren(children.filterNot(_ == child)), child.remove))

}




