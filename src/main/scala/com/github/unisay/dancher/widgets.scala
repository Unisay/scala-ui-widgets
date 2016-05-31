package com.github.unisay.dancher

import com.github.unisay.dancher.dom._

import scalaz.std.list._
import scalaz.syntax.traverse._

case class Paragraph(text: String, id: Option[DomId] = None)
                    (implicit idGen: Gen[DomId])
  extends LeafWidget(id.getOrElse(idGen.generate)) {
  def create = for {
    paragraph ← createElement("p")
    _ ← paragraph setClass "d-paragraph"
    text ← createTextNode(text)
    _ ← paragraph appendChild text
  } yield paragraph
}

case class Label(text: String, id: Option[DomId] = None)(implicit idGen: Gen[DomId])
  extends LeafWidget(id.getOrElse(idGen.generate)) {
  def create = for {
    span ← createElement("span")
    _ ← span setClass "d-label"
    text ← createTextNode(text)
    _ ← span appendChild text
  } yield span

  def setLabel(text: String): (Label, ActionF[_]) = ???
}

case class Button[E](label: String, id: Option[DomId] = None, clickHandler: DomEventHandler = NoEventHandler)
                    (implicit idGen: Gen[DomId])
  extends LeafWidget(id.getOrElse(idGen.generate)) {
  def create = for {
    button ← createElement("button")
    _ ← button setClass "d-button"
    label ← createTextNode(label)
    _ ← button appendChild label
    _ ← button.onClick(clickHandler)
  } yield button
}

abstract class Layout(override val children: Traversable[Widget], id: DomId) extends NodeWidget(id) {
  def create = for {
    div ← createElement("div")
    elements ← children.toList.map(_.create).sequence
    _ ← elements.map(div.appendChild).sequence
  } yield div
}

case class VerticalLayout(override val children: Seq[Widget], id: Option[DomId] = None)
                         (implicit idGen: Gen[DomId])
  extends Layout(children, id.getOrElse(idGen.generate)) {
  override def create = super.create.flatMap(_.setClass("d-vertical-layout"))
}

case class HorizontalLayout(override val children: Seq[Widget], id: Option[DomId] = None)
                           (implicit idGen: Gen[DomId])
  extends Layout(children, id.getOrElse(idGen.generate)) {
  override def create = super.create.flatMap(_.setClass("d-horizontal-layout"))
}
