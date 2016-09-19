package com.github.unisay.dancher

import cats.data.NonEmptyVector
import com.github.unisay.dancher.DomSyntax._
import com.github.unisay.dancher.Widget._
import com.github.unisay.dancher.Fragment._
import fs2.Task.{delay => widget}
import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLInputElement

object Dsl {

  val body = widget(Binding(document.body))
  def body(fragment: Fragment): Widget = body append fragment

  val div = widget(Binding(document.createElement("div")))
  def div(fragment: Fragment): Widget = div append fragment

  def span(text: String) =
    widget {
      val element = document.createElement("span")
      element.appendChild(document.createTextNode(text))
      Binding(element)
    }

  def button(text: String, event: DomainEvent) =
    widget {
      val buttonElement = document.createElement("button")
      buttonElement.setAttribute("type", "button")
      buttonElement.appendChild(document.createTextNode(text))
      Binding(element = buttonElement, events = buttonElement.stream("click").map(_ => event))
    }

  def inputText(placeholder: String = "") =
    widget {
      val inputElement = document.createElement("input").asInstanceOf[HTMLInputElement]
      inputElement.setAttribute("type", "text")
      inputElement.placeholder = placeholder
      Binding(element = inputElement)
    }

  case class Answer(name: String) extends DomainEvent
  def ask(title: String, inputPlaceholder: String = "", buttonCaption: String): Widget = {
    object Click extends DomainEvent
    div {
      div(span(title)) *> div {
        inputText(inputPlaceholder) *> button(buttonCaption, event = Click) map {
          case bindings @ Bindings(NonEmptyVector(input: HTMLInputElement, _), _) =>
            bindings.pipeEvents(_.map(_ => Answer(input.value)))
        }
      }
    }
  }

  def split(vertical: Boolean, left: Fragment, right: Fragment): Widget = {
    val baseClass = "d-split-" + (if (vertical) "vertical" else "horizontal")
    val sideClass = baseClass + "-side"
    val leftHolder = div(left).mapElement(_.setClasses(sideClass, sideClass + "-left"))
    val rightHolder = div(right).mapElement(_.setClasses(sideClass, sideClass + "-right"))
    val splitter = div.mapElement(_.setClass(baseClass + "-handle"))
    div(leftHolder <*> splitter <*> rightHolder).mapElement(_.setClass(baseClass))
  }

  def verticalSplit(left: Widget, right: Widget) = split(vertical = true, left, right)
  def horizontalSplit(left: Widget, right: Widget) = split(vertical = false, left, right)
}
