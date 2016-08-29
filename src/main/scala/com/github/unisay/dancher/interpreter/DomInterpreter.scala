package com.github.unisay.dancher.interpreter

import cats.{Id, ~>}
import com.github.unisay.dancher.dom._
import monix.execution.Cancelable
import monix.reactive.{Observable, OverflowStrategy}
import org.scalajs.dom._

import scala.language.implicitConversions

class DomInterpreter extends ActionInterpreter {

  type DomNodeT = Node
  type DomElemT = Element

  implicit val domNodeEvidence: DomNode[DomNodeT] = new DomNode[DomNodeT] {}
  implicit val domElemEvidence: DomElem[DomElemT] = new DomElem[DomElemT] {}

  def interpret[R, M](model: M, action: ActionF[R]): R = {

    val interpreter = new (Action ~> Id) {

      case class RawNodeList(nodeList: NodeList) extends DomNodeList

      def apply[A](action: Action[A]): Id[A] = {
        implicit def unitToA(unit: Unit): A = unit.asInstanceOf[A]
        implicit def nodeToA(node: Node): A = node.asInstanceOf[A]
        implicit def elementToA(element: Element): A = element.asInstanceOf[A]
        implicit def nodeListToA(nodeList: RawNodeList): A = nodeList.asInstanceOf[A]
        def shouldNotMatch(it: Any): Unit =
          console.error(s"$it should have been matched by the preceding case statements")

        action match {

          case NoAction =>
            debug("NoAction")
            ()

          case Value(a: Any) =>
            debug(s"Value($a)")
            a.asInstanceOf[A]

          case Log(text) =>
            console.info(text)
            ()

          case _: GetDocumentBody[_] =>
            debug("GetDocumentBody")
            document.body

          case GetParent(node: Node) =>
            debug(s"GetParent($node)")
            node.parentNode

          case GetElementById(elementId) =>
            debug(s"GetElementById($elementId)")
            document.getElementById(elementId.value)

          case GetElementsByName(name) =>
            debug(s"GetElementsByName($name)")
            RawNodeList(document.getElementsByName(name))

          case GetElementsByTagName(name) =>
            debug(s"GetElementsByTagName($name)")
            RawNodeList(document.getElementsByName(name))

          case GetElementsByClassName(name) =>
            debug(s"GetElementsByClassName($name)")
            RawNodeList(document.getElementsByName(name))

          case CreateElement(tagName) =>
            debug(s"CreateElement($tagName)")
            document.createElement(tagName)

          case CreateTextNode(text) =>
            debug(s"CreateTextNode($text)")
            document.createTextNode(text)

          case AppendChild(parent: Node, child: Node) =>
            debug(s"AppendChild($parent, $child)")
            parent.asInstanceOf[Element].appendChild(child)
            parent

          case RemoveChild(parent: Node, child: Node) =>
            debug(s"RemoveChild($parent, $child)")
            parent.removeChild(child)
            parent

          case GetFirstChild(node: Node) =>
            debug(s"GetFirstChild($node)")
            node.firstChild

          case ReplaceChild(parent: Node, newChild: Node, oldChild: Node) =>
            debug(s"ReplaceChild($parent, $newChild, $oldChild)")
            parent.replaceChild(newChild, oldChild)
            parent

          case GetAttribute(element: Element, name: String) =>
            debug(s"GetAttribute($element, $name)")
            element.getAttribute(name) match {
              case "" => None
              case null => None
              case value => Some(value)
            }

          case SetAttribute(element: Element, name: String, value: String) =>
            debug(s"SetAttribute($element, $name, $value)")
            element.setAttribute(name, value)
            element

          case HandleEvents(element: Element, eventTypes) =>
            debug(s"HandleEvents($element)")
            Observable.create[(M, DomEventType, Event)](OverflowStrategy.Unbounded) { subscriber =>
              val listeners = eventTypes.map { eventType =>
                eventType.toString.toLowerCase -> ((event: Event) => subscriber.onNext((model, eventType, event)))
              }
              listeners.foreach { case (e, l) => element.addEventListener(e, l) }
              Cancelable(() => listeners.foreach{ case (e, l) => element.removeEventListener(e, l) })
            }.asInstanceOf[A]

          case it@GetParent(_) => shouldNotMatch(it)
          case it@GetFirstChild(_) => shouldNotMatch(it)
          case it@AppendChild(_, _) => shouldNotMatch(it)
          case it@RemoveChild(_, _) => shouldNotMatch(it)
          case it@ReplaceChild(_, _, _) => shouldNotMatch(it)
          case it@GetAttribute(_, _) => shouldNotMatch(it)
          case it@SetAttribute(_, _, _) => shouldNotMatch(it)
          case it@HandleEvents(_, _) => shouldNotMatch(it)
        }
      }

    }

    action.foldMap(interpreter)
  }

  private def debug(message: => String) = console.info(message)
}
