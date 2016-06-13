package com.github.unisay.dancher

import com.github.unisay.dancher.dom._
import cats.std.list._
import cats.syntax.traverse._
import monocle.macros.{GenLens, GenPrism}

case class Body(override val domId: DomId) extends NodeWidget(domId) {
  def children: Traversable[Widget] = Seq.empty
  def create: ActionF[DomElement] = getDocumentBody
}

case class Paragraph(text: String, id: Option[DomId] = None)
                    (implicit idGen: Generator[DomId])
  extends LeafWidget(id.getOrElse(idGen.generate)) {
  def create = for {
    paragraph ← createElement("p")
    _ ← paragraph setClass "d-paragraph"
    text ← createTextNode(text)
    _ ← paragraph appendChild text
  } yield paragraph
}

case class Label(override val domId: DomId, text: String) extends LeafWidget(domId) {
  def create = for {
    span ← createElement("span")
    _ ← span setId domId
    _ ← span setClass "d-label"
    text ← createTextNode(text)
    _ ← span appendChild text
  } yield span

  def setText(textToSet: String): (Label, ActionF[_]) = {
    val updatedLabel = copy(text = textToSet)
    val action = for {
      span ← element
      oldChild ← span.getFirstChild
      newChild ← createTextNode(textToSet)
      _ ← span.replaceChild(newChild, oldChild)
    } yield span
    (updatedLabel, action)
  }
}

case class Button(override val domId: DomId, label: String, clickHandler: DomEventHandler = NoEventHandler)
  extends LeafWidget(domId) {
  def create = for {
    button ← createElement("button")
    _ ← button setId domId
    _ ← button setClass "d-button"
    text ← createTextNode(label)
    _ ← button appendChild text
    _ ← button.onClick(clickHandler)
  } yield button
}

abstract class HasChildren(override val domId: DomId, override val children: Traversable[Widget])
  extends NodeWidget(domId) {

  def create = for { // TODO remember created element by returning a copy of the widget
    div ← createElement("div")
    _ ← div setId domId
    elements ← children.toList.map(_.create).sequence
    _ ← elements.map(div.appendChild).sequence
  } yield div

}

object VerticalLayout {
  val _children = GenPrism[Widget, VerticalLayout] composeLens GenLens[VerticalLayout](_.children)
}

case class VerticalLayout(override val domId: DomId, override val children: Vector[Widget])
  extends HasChildren(domId, children) {
  override def create = super.create.flatMap(_.setClass("d-vertical-layout"))
  def removeChild(id: DomId): (VerticalLayout, ActionF[DomNode]) =
    children.find(_.domId == id)
      .map(child ⇒ (copy(children = children.filterNot(_ == child)), child.remove))
      .getOrElse((this, node))
}

object HorizontalLayout {
  val _children = GenPrism[Widget, HorizontalLayout] composeLens GenLens[HorizontalLayout](_.children)
}

case class HorizontalLayout(override val domId: DomId, override val children: Vector[Widget])
  extends HasChildren(domId, children) {
  override def create = super.create.flatMap(_.setClass("d-horizontal-layout"))
  def removeChild(id: DomId): (HorizontalLayout, ActionF[DomNode]) =
    children.find(_.domId == id)
      .map(child ⇒ (copy(children = children.filterNot(_ == child)), child.remove))
      .getOrElse((this, node))
}
