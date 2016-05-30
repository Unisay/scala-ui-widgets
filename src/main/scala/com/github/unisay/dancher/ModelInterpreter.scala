package com.github.unisay.dancher

import scalaz.Id._
import scalaz.~>
import org.scalajs.dom.document
import org.scalajs.dom.console
import org.scalajs.dom.raw
import dom._

class ModelInterpreter {

  def interpret[E](model: Model, action: ActionF[Unit])(update: (E, Model) ⇒ (Model, ActionF[Unit])): Unit =
    action.foldMap(new (Action ~> Id) {

      case class RawNode(node: raw.Node) extends DomNode
      case class RawElement(element: raw.Element) extends DomElement
      case class RawNodeList(nodeList: raw.NodeList) extends DomNodeList
      case class RawMouseEvent(event: raw.MouseEvent) extends DomMouseEvent

      def apply[B](action: Action[B]): Id[B] = action match {

        case NoAction ⇒
          ()

        case Log(text) ⇒
          console.info(text)
          ()

        case GetDocumentBody(elementToNext) ⇒
          elementToNext(RawElement(document.body))

        case GetParent(RawNode(node), nodeToNext) ⇒
          nodeToNext(RawNode(node.parentNode))

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

        case RemoveChild(parent, child, next) ⇒
          parent.removeChild(child)
          next

        case ReplaceChild(parent, oldChild, newChild, next) ⇒
          parent.replaceChild(oldChild, newChild)
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
          element.addEventListener("click", (mouseEvent: raw.MouseEvent) ⇒ {
            val event: DomMouseEvent = RawMouseEvent(mouseEvent)
            val context: E = handler.asInstanceOf[DomMouseEventHandler[E]](event)
            val (updatedModel, updateAction) = update(context, model)
            interpret(updatedModel, updateAction)(update)
          })
          next

        case it@GetParent(_, _) ⇒ shouldNotMatch(it)
        case it@AppendChild(_, _, _) ⇒ shouldNotMatch(it)
        case it@SetAttribute(_, _, _, _) ⇒ shouldNotMatch(it)
        case it@SetOnClick(_, _, _) ⇒ shouldNotMatch(it)
      }

      def shouldNotMatch(it: Any) =
        sys.error(s"$it should have been matched by the preceding case statements")
    })
}
