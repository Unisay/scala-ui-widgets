package com.github.unisay.dancher

import scalaz.Id._
import scalaz.~>
import org.scalajs.dom.document._
import org.scalajs.dom.console

class ActionRunner {

  def run[A](domAction: FAction[A]): A = domAction.runM(domInterpreter)

  private val domInterpreter: Action ~> Id = new (Action ~> Id) {
    def apply[A](action: Action[A]): Id[A] = action match {

      case Log(text, next) ⇒
        console.info(text)
        next

      case GetElementById(elementId, elementToNext) ⇒
        elementToNext(getElementById(elementId))

      case CreateElement(tagName, elementToNext) ⇒
        elementToNext(createElement(tagName))

      case CreateTextNode(text, textToNext) ⇒
        textToNext(createTextNode(text))

      case AppendChild(parent, child, next) ⇒
        parent.appendChild(child)
        next
    }
  }

}