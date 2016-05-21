package com.github.unisay.dancher

import com.github.unisay.dancher.Action._

import scalaz.Free

trait Widget {
  def create: ActionF[DomElement]
}

abstract class ContainerWidget(val children: Widget*) extends Widget

case class Paragraph(text: String) extends Widget {
  def create = for {
    paragraph ← createElement("p")
    text ← createTextNode(text)
    _ ← paragraph appendChild text
  } yield paragraph
}

case class Button(label: String, clickHandler: Option[MouseEventHandler] = None) extends Widget {
  def create = {
    for {
      button ← createElement("button")
      _ ← button setClass "d-button"
      label ← createTextNode(label)
      _ ← button appendChild label
      _ ← clickHandler.fold(Free.pure[Action, Unit](()))(button.onClick)
    } yield button
  }
}

case class Holder(widget: Widget) extends ContainerWidget(widget) {
  def create = for {
    child ← widget.create
    _ ← child setClass "d-holder-child"
    div ← createElement("div")
    _ ← div setClass "d-holder"
    _ ← div appendChild child
  } yield div
}