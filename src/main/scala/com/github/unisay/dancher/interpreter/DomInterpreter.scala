package com.github.unisay.dancher.interpreter

import cats.{Id, ~>}
import com.github.unisay.dancher._
import com.github.unisay.dancher.dom._
import monix.execution.Cancelable
import monix.reactive.{Observable, OverflowStrategy}
import org.scalajs.dom.raw.MouseEvent
import org.scalajs.dom.{console, document, raw}

import scala.language.implicitConversions

class DomInterpreter {

  def interpret[T](model: Model, action: ActionF[T]): T = {

    val compiler = new (Action ~> Id) {

      case class RawNode(node: raw.Node) extends DomNode
      case class RawElement(element: raw.Element) extends DomElement
      case class RawNodeList(nodeList: raw.NodeList) extends DomNodeList
      case class RawMouseEvent(event: raw.MouseEvent) extends DomMouseEvent

      def apply[A](action: Action[A]): Id[A] = {
        implicit def nodeToA(r: RawNode): A = r.asInstanceOf[A]
        implicit def elementToA(r: RawElement): A = r.asInstanceOf[A]
        implicit def nodeListToA(r: RawNodeList): A = r.asInstanceOf[A]

        action match {

          case NoAction ⇒
            ().asInstanceOf[A]

          case Log(text) ⇒
            console.info(text)
            ().asInstanceOf[A]

          case GetDocumentBody ⇒
            RawElement(document.body)

          case GetParent(RawElement(node)) ⇒
            RawNode(node.parentNode)

          case GetParent(RawNode(node)) ⇒
            RawNode(node.parentNode)

          case GetElementById(elementId) ⇒
            RawElement(document.getElementById(elementId.value))

          case GetElementsByName(name) ⇒
            RawNodeList(document.getElementsByName(name))

          case GetElementsByTagName(name) ⇒
            RawNodeList(document.getElementsByName(name))

          case GetElementsByClassName(name) ⇒
            RawNodeList(document.getElementsByName(name))

          case CreateElement(tagName) ⇒
            RawElement(document.createElement(tagName)).asInstanceOf[A]

          case CreateTextNode(text) ⇒
            RawNode(document.createTextNode(text)).asInstanceOf[A]

          case AppendChild(rawParent@RawElement(parent), RawNode(child)) ⇒
            parent.appendChild(child)
            rawParent

          case RemoveChild(rawParent@RawElement(parent), RawNode(child)) ⇒
            parent.removeChild(child)
            rawParent

          // TODO-remove duplication
          case RemoveChild(rawParent@RawNode(parent), RawNode(child)) ⇒
            parent.removeChild(child)
            rawParent

          // TODO-remove duplication
          case RemoveChild(rawParent@RawNode(parent), RawElement(child)) ⇒
            parent.removeChild(child)
            rawParent

          case GetFirstChild(RawNode(node)) ⇒
            RawNode(node.firstChild)

          case GetFirstChild(RawElement(node)) ⇒
            RawNode(node.firstChild)

          case ReplaceChild(rawParent@RawElement(parent), RawNode(newChild), RawNode(oldChild)) ⇒
            parent.replaceChild(newChild, oldChild)
            rawParent

          // Has to provide separate case for RawElement child,
          // because if RawElement is made a subclass of RawNode
          // then custom unapply breaks the exhaustiveness checker
          // https://issues.scala-lang.org/browse/SI-8511
          case AppendChild(rawParent@RawElement(parent), RawElement(child)) ⇒
            parent.appendChild(child)
            rawParent

          case SetAttribute(rawElement@RawElement(element), name, value) ⇒
            element.setAttribute(name, value)
            rawElement

          case SetOnClick(rawElement@RawElement(element), domEventHandler) ⇒
            // TODO: choose appropriate overflow strategy
            Observable.create[ModelEvent](OverflowStrategy.DropNew(10)) { subscriber ⇒
              val listener = (mouseEvent: MouseEvent) ⇒ {
                val domainEvent = domEventHandler(RawMouseEvent(mouseEvent))
                subscriber.onNext((domainEvent, model))
                ()
              }
              element.addEventListener("click", listener)
              Cancelable(() ⇒ element.removeEventListener("click", listener))
            }

          case it@GetParent(_) ⇒ shouldNotMatch(it)
          case it@GetFirstChild(_) ⇒ shouldNotMatch(it)
          case it@AppendChild(_, _) ⇒ shouldNotMatch(it)
          case it@RemoveChild(_, _) ⇒ shouldNotMatch(it)
          case it@ReplaceChild(_, _, _) ⇒ shouldNotMatch(it)
          case it@SetAttribute(_, _, _) ⇒ shouldNotMatch(it)
          case it@SetOnClick(_, _) ⇒ shouldNotMatch(it)
        }
      }

      def shouldNotMatch(it: Any) =
        sys.error(s"$it should have been matched by the preceding case statements")
    }

    action.foldMap(compiler)
  }
}
