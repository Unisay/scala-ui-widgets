package com.github.unisay.dancher

import scalaz.Id._
import scalaz.~>
import org.scalajs.dom.document
import org.scalajs.dom.console
import org.scalajs.dom.raw
import Action._

import scala.language.implicitConversions

class DomInterpreter extends ActionInterpreter {

  def interpret[A](actionF: ActionF[A]): A = actionF.foldMap(domInterpreter)

  private val domInterpreter: Action ~> Id = new (Action ~> Id) {

    case class RawNode(node: raw.Node) extends DomNode
    case class RawElement(element: raw.Element) extends DomElement
    case class RawNodeList(nodeList: raw.NodeList) extends DomNodeList
    case class RawMouseEvent(event: raw.MouseEvent) extends DomMouseEvent

    def apply[A](action: Action[A]): Id[A] = action match {

      case Log(text, next) ⇒
        console.info(text)
        next

      case GetDocumentBody(elementToNext) ⇒
        elementToNext(RawElement(document.body))

      case GetElementById(elementId, elementToNext) ⇒
        elementToNext(RawElement(document.getElementById(elementId)))

      case GetElementsByName(name, nodeListToNext) ⇒
        nodeListToNext(RawNodeList(document.getElementsByName(name)))

      case GetElementsByTagName(name, nodeListToNext) ⇒
        nodeListToNext(RawNodeList(document.getElementsByName(name)))

      case GetElementsByClassName(name, nodeListToNext) ⇒
        nodeListToNext(RawNodeList(document.getElementsByName(name)))

      case CreateElement(tagName, elementToNext) ⇒
        elementToNext(RawElement(document.createElement(tagName)))

      case CreateTextNode(text, textToNext) ⇒
        textToNext(RawNode(document.createTextNode(text)))

      case AppendChild(RawElement(parent), RawNode(child), next) ⇒
        parent.appendChild(child)
        next

      // Has to provide separate case for RawElement child,
      // because if RawElement is made a subclass of RawNode
      // then custom unapply breaks the exhaustiveness checker
      // https://issues.scala-lang.org/browse/SI-8511
      case AppendChild(RawElement(parent), RawElement(child), next) ⇒
        parent.appendChild(child)
        next

      case SetAttribute(RawElement(element), name, value, next) ⇒
        element.setAttribute(name, value)
        next

      case SetOnClick(RawElement(element), handler, next) ⇒
        element.addEventListener("click", (mouseEvent: raw.MouseEvent) ⇒ handler(RawMouseEvent(mouseEvent)))
        next

      case it @ AppendChild(_, _, _) ⇒ shouldNotMatch(it)
      case it @ SetAttribute(_, _, _, _) ⇒ shouldNotMatch(it)
      case it @ SetOnClick(_, _, _) ⇒ shouldNotMatch(it)
    }

    def shouldNotMatch(it: Any) =
      sys.error(s"$it should have been matched by the preceding case statements")
  }
}