package com.github.unisay.dancher.widget

import com.github.unisay.dancher.Dom.Event.Click
import com.github.unisay.dancher.DomSyntax._
import com.github.unisay.dancher.Widget._
import com.github.unisay.dancher._
import fs2.Task.{delay => widget}
import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLInputElement

object BasicWidgets {

  trait WidgetContainer {
    def apply(): Widget
    def fromElement(element: Element): Widget = widget(Binding(element))
    def apply(fragment: Fragment): Widget = apply() appendFragment fragment
  }

  object body extends WidgetContainer {
    def apply() = fromElement(document.body)
  }

  object div extends WidgetContainer {
    def apply() = fromElement(document.createElement("div"))
  }

  def span(text: String): Widget =
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
      Binding(element = buttonElement, domEvents = buttonElement.stream(Click))
    }

  def inputText(placeholder: String = "") =
    widget {
      val inputElement = document.createElement("input").asInstanceOf[HTMLInputElement]
      inputElement.setAttribute("type", "text")
      inputElement.placeholder = placeholder
      Binding(element = inputElement)
    }

}
