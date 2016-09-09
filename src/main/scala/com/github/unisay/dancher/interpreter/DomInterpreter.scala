package com.github.unisay.dancher.interpreter

import cats.data.Ior
import cats.{Id, ~>}
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.widget.EffectAction
import monix.execution.Cancelable
import monix.reactive.{Observable, OverflowStrategy}
import org.scalajs.dom._

import scala.language.implicitConversions

class DomInterpreter extends ActionInterpreter {

  def makeEvent(eventType: DomEventType, event: Event): DomEvent Ior EffectAction =
    Ior.Left {
      eventType match {
        case Click => new ClickEvent {
          val mouseEvent = event.asInstanceOf[org.scalajs.dom.MouseEvent]
          val page   = Vector2d(mouseEvent.pageX,   mouseEvent.pageY)
          val screen = Vector2d(mouseEvent.screenX, mouseEvent.screenY)
          val client = Vector2d(mouseEvent.clientX, mouseEvent.clientY)
        }
        case MouseEnter => new MouseEnterEvent {
          val mouseEvent = event.asInstanceOf[org.scalajs.dom.MouseEvent]
          val page   = Vector2d(mouseEvent.pageX,   mouseEvent.pageY)
          val screen = Vector2d(mouseEvent.screenX, mouseEvent.screenY)
          val client = Vector2d(mouseEvent.clientX, mouseEvent.clientY)
        }
        case MouseLeave => new MouseLeaveEvent {
          val mouseEvent = event.asInstanceOf[org.scalajs.dom.MouseEvent]
          val page   = Vector2d(mouseEvent.pageX,   mouseEvent.pageY)
          val screen = Vector2d(mouseEvent.screenX, mouseEvent.screenY)
          val client = Vector2d(mouseEvent.clientX, mouseEvent.clientY)
        }
        case MouseUp    => new MouseUpEvent    {
          val mouseEvent = event.asInstanceOf[org.scalajs.dom.MouseEvent]
          val page   = Vector2d(mouseEvent.pageX,   mouseEvent.pageY)
          val screen = Vector2d(mouseEvent.screenX, mouseEvent.screenY)
          val client = Vector2d(mouseEvent.clientX, mouseEvent.clientY)
        }
        case MouseDown  => new MouseDownEvent  {
          val mouseEvent = event.asInstanceOf[org.scalajs.dom.MouseEvent]
          val page   = Vector2d(mouseEvent.pageX,   mouseEvent.pageY)
          val screen = Vector2d(mouseEvent.screenX, mouseEvent.screenY)
          val client = Vector2d(mouseEvent.clientX, mouseEvent.clientY)
        }
        case MouseMove  => new MouseMoveEvent  {
          val mouseEvent = event.asInstanceOf[org.scalajs.dom.MouseEvent]
          val page   = Vector2d(mouseEvent.pageX,   mouseEvent.pageY)
          val screen = Vector2d(mouseEvent.screenX, mouseEvent.screenY)
          val client = Vector2d(mouseEvent.clientX, mouseEvent.clientY)
        }
      }
    }

  def interpret[R, M](model: M, action: ActionF[R]): R = {

    val interpreter = new (Action ~> Id) {

      def apply[A](action: Action[A]): Id[A] = {
        implicit def unitToA(unit: Unit): A = unit.asInstanceOf[A]
        implicit def nodeToA(node: Node): A = node.asInstanceOf[A]
        implicit def elementToA(element: Element): A = element.asInstanceOf[A]
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

          case RenderScalaTag(tag) =>
            debug(s"RenderScalaTag(${tag.tag})")
            tag.render

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

          case ReplaceChild(parent: Node, newChild: Node, oldChild: Node) =>
            debug(s"ReplaceChild($parent, $newChild, $oldChild)")
            parent.replaceChild(newChild, oldChild)
            parent

          case SetAttribute(element: Element, name: String, value: String) =>
            debug(s"SetAttribute($element, $name, $value)")
            element.setAttribute(name, value)
            element

          case HandleEvents(element: Element, eventTypes) =>
            debug(s"HandleEvents($element)")
            Observable.create[DomEvent Ior EffectAction](OverflowStrategy.Unbounded) { subscriber =>
              val listeners = eventTypes.map { eventType =>
                eventType.toString.toLowerCase -> ((event: Event) => subscriber.onNext(makeEvent(eventType, event)))
              }
              listeners.foreach { case (e, l) => element.addEventListener(e, l) }
              Cancelable(() => listeners.foreach{ case (e, l) => element.removeEventListener(e, l) })
            }.asInstanceOf[A]

          case it@AppendChild(_, _) => shouldNotMatch(it)
          case it@RemoveChild(_, _) => shouldNotMatch(it)
          case it@ReplaceChild(_, _, _) => shouldNotMatch(it)
          case it@SetAttribute(_, _, _) => shouldNotMatch(it)
          case it@HandleEvents(_, _) => shouldNotMatch(it)
        }
      }

    }

    action.foldMap(interpreter)
  }

  private def debug(message: => String) = console.info(message)
}
