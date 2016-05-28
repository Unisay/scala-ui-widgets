package com.github.unisay.dancher

import com.github.unisay.dancher.DomAction._

import scalaz.Free
import scalaz.std.list._
import scalaz.syntax.traverse._

case class Paragraph(text: String, id: Option[DomId] = None)
                    (implicit idGen: Gen[DomId])
  extends Model(id.getOrElse(idGen.generate)) {
  def create = for {
    paragraph ← createElement("p")
    _ ← paragraph setClass "d-paragraph"
    text ← createTextNode(text)
    _ ← paragraph appendChild text
  } yield paragraph
}

case class Label(text: String, id: Option[DomId] = None)
                (implicit idGen: Gen[DomId])
  extends Model(id.getOrElse(idGen.generate)) {
  def create = for {
    span ← createElement("span")
    _ ← span setClass "d-label"
    text ← createTextNode(text)
    _ ← span appendChild text
  } yield span
}

case class Button[E](label: String,
                     id: Option[DomId] = None,
                     onClick: Option[DomMouseEventHandler[E]] = None)
                    (implicit idGen: Gen[DomId])
  extends Model(id.getOrElse(idGen.generate)) {
  def create = for {
    button ← createElement("button")
    _ ← button setClass "d-button"
    label ← createTextNode(label)
    _ ← button appendChild label
    _ ← onClick.fold(Free.pure[DomAction, DomElement](button))(button.onClick)
  } yield button
}

abstract class Layout(models: Seq[Model], id: DomId) extends Model(id) {
  def create: Free[DomAction, DomElement] = for {
    div ← createElement("div")
    elements ← models.toList.map(_.create).sequence
    _ ← elements.map(div.appendChild).sequence
  } yield div
}

case class VerticalLayout(models: Seq[Model], id: Option[DomId] = None)(implicit idGen: Gen[DomId])
  extends Layout(models, id.getOrElse(idGen.generate)) {
  override def create = super.create.flatMap(_.setClass("d-vertical-layout"))
}

case class HorizontalLayout(models: Seq[Model], id: Option[DomId] = None)(implicit idGen: Gen[DomId])
  extends Layout(models, id.getOrElse(idGen.generate)) {
  override def create = super.create.flatMap(_.setClass("d-horizontal-layout"))
}
