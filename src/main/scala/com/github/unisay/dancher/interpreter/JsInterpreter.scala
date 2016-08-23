package com.github.unisay.dancher.interpreter

import cats._
import cats.data.State
import cats.instances.all._
import cats.syntax.eq._
import com.github.unisay.dancher.dom._
import monix.reactive.Observable

import scala.language.implicitConversions

object JsInterpreter {

  case class RawElement(element: String) extends DomElement
  case class RawNodeList(nodeList: String) extends DomNodeList
  case class RawMouseEvent(event: String) extends DomMouseEvent
  case class RawNode(node: String) extends DomNode

  type Script = List[String]
  type Result = (Map[String, Int], Script)
  type InterpreterState[A] = State[Result, A]

  def interpret[R, M](model: M, action: ActionF[R],
                      domEvents: Observable[(String, DomEvent)] = Observable.empty): (R, Script) = {
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
        implicit def toA(aw: InterpreterState[_]): InterpreterState[A] = aw.asInstanceOf[InterpreterState[A]]

        action match {

          case NoAction =>
            noModification

          case Value(a: Any) =>
            result(a.asInstanceOf[A])

          case GetDocumentBody =>
            scriptWithReturn("var b = document.body", RawElement("b"))

          case GetParent(RawNode(node)) =>
            result(RawNode(s"$node.parentNode"))

          case GetParent(RawElement(node)) =>
            result(RawNode(s"$node.parentNode"))

          case GetElementById(elementId) =>
            scriptWithCounterAndReturn[RawElement]("element") { variableName =>
              (s"var $variableName = document.getElementById('${elementId.value}')", RawElement(variableName))
            }

          case GetElementsByName(name) =>
            result(RawNodeList(s"document.getElementsByName('$name')"))

          case GetElementsByTagName(name) =>
            result(RawNodeList(s"document.getElementsByName('$name')"))

          case GetElementsByClassName(name) =>
            result(RawNodeList(s"document.getElementsByName('$name')"))

          case CreateElement(tagName) =>
            scriptWithCounterAndReturn[RawElement](tagName) { variableName =>
              (s"var $variableName = document.createElement('$tagName')", RawElement(variableName))
            }

          case CreateTextNode(text) =>
            scriptWithCounterAndReturn[RawNode]("text") { variableName =>
              (s"var $variableName = document.createTextNode('$text')", RawNode(variableName))
            }

          case AppendChild(rawParent@RawElement(parent), RawNode(child)) =>
            scriptWithReturn[RawElement](s"$parent.appendChild($child)", rawParent)

          case AppendChild(rawParent@RawElement(parent), RawElement(child)) =>
            scriptWithReturn[RawElement](s"$parent.appendChild($child)", rawParent)


          case RemoveChild(rawParent@RawElement(parent), RawNode(child)) =>
            scriptWithReturn(s"$parent.removeChild($child)", rawParent)

          case RemoveChild(rawParent@RawNode(parent), RawElement(child)) =>
            scriptWithReturn(s"$parent.removeChild($child)", rawParent)

          case GetFirstChild(RawNode(node)) =>
            result(RawNode(s"$node.firstChild"))

          case GetFirstChild(RawElement(node)) =>
            result(RawNode(s"$node.firstChild"))

          case ReplaceChild(rawParent@RawElement(parent), RawNode(newChild), RawNode(oldChild)) =>
            scriptWithReturn(s"$parent.replaceChild($newChild, $oldChild)", rawParent)

          case SetAttribute(rawElement@RawElement(element), name, value) =>
            scriptWithReturn(s"$element.setAttribute('$name', '$value')", rawElement)

          case SetOnClick(rawElement@RawElement(element), domEventHandler) =>
            val result = domEvents
              .flatMap {
                case (el, domEvent) if el === element => Observable(domEvent)
                case _ => Observable.empty
              }
              .flatMap(domEventHandler.asInstanceOf[DomEventHandler[M]])
            scriptWithReturn(s"/* SetOnClick($element) */", result)

          case it =>
            sys.error(s"Can't compile to JS: $it")

        }
      }
    }

    val interpreterState = action.foldMap(interpreter)
    val ((_, reverseScript), a) = interpreterState.run((Map.empty, Nil)).value
    (a, reverseScript.reverse)
  }

}
