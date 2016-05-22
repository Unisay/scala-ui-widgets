package com.github.unisay.dancher

import com.github.unisay.dancher.Action._

import scalaz.Free
import scalaz.std.list._
import scalaz.syntax.traverse._

trait Widget {
  def create: ActionF[DomElement]
}

case class Paragraph(text: String) extends Widget {
  def create = for {
    paragraph ← createElement("p")
    text ← createTextNode(text)
    _ ← paragraph appendChild text
  } yield paragraph
}

case class Button(label: String, clickHandler: Option[MouseEventHandler] = None) extends Widget {
  def create = for {
    button ← createElement("button")
    _ ← button setClass "d-button"
    label ← createTextNode(label)
    _ ← button appendChild label
    _ ← clickHandler.fold(Free.pure[Action, DomElement](button))(button.onClick)
  } yield button
}

case class Holder(widget: Widget) {
  def create = for {
    child ← widget.create
    _ ← child setClass "d-holder-child"
    div ← createElement("div")
    _ ← div setClass "d-holder"
    _ ← div appendChild child
  } yield div
}

abstract class Layout(widgets: Widget*) extends Widget {
  def create: Free[Action, DomElement] = for {
    div ← createElement("div")
    elements ← widgets.toList.map(_.create).sequence
    _ ← elements.map(div.appendChild).sequence
  } yield div
}

case class VerticalLayout(widgets: Widget*) extends Layout(widgets: _*) {
  override def create = super.create.flatMap(_.setClass("d-vertical-layout"))
}

case class HorizontalLayout(widgets: Widget*) extends Layout(widgets: _*) {
  override def create = super.create.flatMap(_.setClass("d-horizontal-layout"))
}