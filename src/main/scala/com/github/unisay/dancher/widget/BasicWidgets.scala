package com.github.unisay.dancher.widget

import cats.syntax.xor._
import com.github.unisay.dancher.Dom.Event.Click
import com.github.unisay.dancher.DomSyntax._
import com.github.unisay.dancher.Widget._
import com.github.unisay.dancher._
import fs2.Task
import fs2.Task.{delay => widget}
import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLInputElement

object BasicWidgets {

  val body = widget(Binding(document.body))
  def body(widget: Widget): Widget = body append widget
  def body(widgets: List[Widget]): Widget = body append widgets

  val div = widget(Binding(document.createElement("div")))
  def div(widget: Widget): Widget = div append widget
  def div(widgets: List[Widget]): Widget = div append widgets
  def div(widgets: Task[List[Binding]]): Widget = div append widgets

  def span(text: String) =
    widget {
      val element = document.createElement("span")
      element.appendChild(document.createTextNode(text))
      Binding(element)
    }

  def button(text: String) =
    widget {
      val buttonElement = document.createElement("button")
      buttonElement.setAttribute("type", "button")
      buttonElement.appendChild(document.createTextNode(text))
      Binding(element = buttonElement, events = buttonElement.stream(Click).map(_.left))
    }

  def inputText(placeholder: String = "") =
    widget {
      val inputElement = document.createElement("input").asInstanceOf[HTMLInputElement]
      inputElement.setAttribute("type", "text")
      inputElement.placeholder = placeholder
      Binding(element = inputElement)
    }

}
