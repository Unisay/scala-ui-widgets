package com.github.unisay.dancher.interpreter

import cats._
import cats.data.{Ior, State}
import cats.instances.all._
import cats.syntax.eq._
import com.github.unisay.dancher.DomainEvent
import com.github.unisay.dancher.dom._
import monix.reactive.Observable

import scala.language.implicitConversions

object JsInterpreter extends ActionInterpreter {

  type DomNodeT = JsInterpreterNode
  type DomElemT = JsInterpreterElement
  implicit val domElemEvidence: DomElem[DomElemT] = new DomElem[DomElemT] {}

  case class EffectActionEvent(effect: String) extends DomainEvent
  case class JsInterpreterNode(name: String)
  case class JsInterpreterElement(name: String)
  case class RawNodeList(nodeList: String) extends DomNodeList
  case class RawMouseEvent(element: JsInterpreterElement, event: String) extends DomMouseEvent

  type Script = List[String]
  type Result = (Map[String, Int], Script)
  type InterpreterState[A] = State[Result, A]

  def interpret[R, M](model: M, action: ActionF[R]): R = interpretScript(model, action, Observable.empty)._1

  def interpretScript[R, M](model: M, action: ActionF[R],
                            domEvents: Observable[(String, (DomEventType, DomEvent))] = Observable.empty,
                            attributes: Map[JsInterpreterElement, Map[String, String]] = Map.empty): (R, String) = {
    val interpreter: Action ~> InterpreterState = new (Action ~> InterpreterState) {

      def result[A](a: A): InterpreterState[A] =
        State.pure(a)

      def commentWithReturn[A](comment: String, a: A): InterpreterState[A] =
        scriptWithReturn(s"/* $comment */", a)

      def noModification: InterpreterState[Unit] =
        State.modify[Result](identity)

      def comment(c: String): InterpreterState[Unit] =
        State.modify { case (counters, script) => (counters, s"/* $c */" :: script) }

      def scriptWithReturn[A](line: String, result: A): InterpreterState[A] =
        State.modify[Result] { case (counters, script) => (counters, line :: script) }.map(_ => result)

      def scriptWithCounterAndReturn[A](element: String)(f: String => (String, A)): InterpreterState[A] =
        State { case (counters, script) =>
          val counter = counters.getOrElse(element, 0)
          val (line, a) = f(element + counter)
          ((counters.updated(element, counter + 1), line :: script), a)
        }

      override def apply[A](action: Action[A]): InterpreterState[A] = {
        implicit def nodeToA(node: JsInterpreterNode): A = node.asInstanceOf[A]
        implicit def elementToA(element: JsInterpreterElement): A = element.asInstanceOf[A]
        implicit def toA(aw: InterpreterState[_]): InterpreterState[A] = aw.asInstanceOf[InterpreterState[A]]

        action match {

          case NoAction =>
            noModification

          case Value(a: Any) =>
            result(a.asInstanceOf[A])

          case _: GetDocumentBody[_] =>
            scriptWithReturn("var b = document.body", JsInterpreterElement("b"))

          case GetParent(JsInterpreterNode(node)) =>
            result(JsInterpreterNode(s"$node.parentNode"))

          case GetParent(JsInterpreterElement(node)) =>
            result(JsInterpreterNode(s"$node.parentNode"))

          case GetElementById(elementId) =>
            scriptWithCounterAndReturn[JsInterpreterElement]("element") { variableName =>
              (s"var $variableName = document.getElementById('${elementId.value}')", JsInterpreterElement(variableName))
            }

          case GetElementsByName(name) =>
            result(RawNodeList(s"document.getElementsByName('$name')"))

          case GetElementsByTagName(name) =>
            result(RawNodeList(s"document.getElementsByName('$name')"))

          case GetElementsByClassName(name) =>
            result(RawNodeList(s"document.getElementsByName('$name')"))

          case CreateElement(tagName) =>
            scriptWithCounterAndReturn[JsInterpreterElement](tagName) { variableName =>
              (s"var $variableName = document.createElement('$tagName')", JsInterpreterElement(variableName))
            }

          case CreateTextNode(text) =>
            scriptWithCounterAndReturn[JsInterpreterNode]("text") { variableName =>
              (s"var $variableName = document.createTextNode('$text')", JsInterpreterNode(variableName))
            }

          case AppendChild(rawParent@JsInterpreterElement(parent), JsInterpreterNode(child)) =>
            scriptWithReturn[JsInterpreterElement](s"$parent.appendChild($child)", rawParent)

          case AppendChild(rawParent@JsInterpreterElement(parent), JsInterpreterElement(child)) =>
            scriptWithReturn[JsInterpreterElement](s"$parent.appendChild($child)", rawParent)


          case RemoveChild(rawParent@JsInterpreterElement(parent), JsInterpreterNode(child)) =>
            scriptWithReturn(s"$parent.removeChild($child)", rawParent)

          case RemoveChild(rawParent@JsInterpreterNode(parent), JsInterpreterElement(child)) =>
            scriptWithReturn(s"$parent.removeChild($child)", rawParent)

          case GetFirstChild(JsInterpreterNode(node)) =>
            result(JsInterpreterNode(s"$node.firstChild"))

          case GetFirstChild(JsInterpreterElement(node)) =>
            result(JsInterpreterNode(s"$node.firstChild"))

          case ReplaceChild(rawParent@JsInterpreterElement(parent), JsInterpreterNode(newChild), JsInterpreterNode(oldChild)) =>
            scriptWithReturn(s"$parent.replaceChild($newChild, $oldChild)", rawParent)

          case GetAttribute(rawElement@JsInterpreterElement(element), name) =>
            val value = for {
              elementAttributes <- attributes.get(rawElement)
              attributeValue <- elementAttributes.get(name)
            } yield attributeValue
            scriptWithReturn(s"""$element.getAttribute("$name")""", value)

          case SetAttribute(rawElement@JsInterpreterElement(element), name, value) =>
            scriptWithReturn(s"""$element.setAttribute("$name", "$value")""", rawElement)

          case HandleEvents(JsInterpreterElement(element), domEventHandlers) =>
            val result = domEvents
              .flatMap {
                case (el, domEvent) if el === element => Observable(domEvent)
                case _ => Observable.empty
              }
              .flatMap {
                case (eventType, event) =>
                  val handlers = domEventHandlers.asInstanceOf[DomEventHandlers[M]]
                  Observable.fromIterable(handlers.get(eventType).map(_ (model, event)).toList).flatten
              }
              .flatMap {
                case (m, Ior.Right(domainEvent)) =>
                  Observable((m, domainEvent))
                case (_, Ior.Left(actionF)) =>
                  Observable((null, EffectActionEvent(interpretScript(model, actionF)._2)))
                case (m, Ior.Both(actionF, domainEvent)) =>
                  Observable((m, EffectActionEvent(interpretScript(model, actionF)._2)), (m, domainEvent))
              }
            scriptWithReturn(s"/* HandleEvents($element) */", result)

          case it =>
            sys.error(s"Can't compile to JS: $it")

        }
      }
    }

    val interpreterState = action.foldMap(interpreter)
    val ((_, reverseScript), a) = interpreterState.run((Map.empty, Nil)).value
    (a, reverseScript.reverse.mkString("", ";\n", ";").trim)
  }

}
