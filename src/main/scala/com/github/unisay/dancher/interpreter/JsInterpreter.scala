package com.github.unisay.dancher.interpreter

import cats._
import cats.data.{Ior, State}
import cats.instances.all._
import cats.syntax.eq._
import com.github.unisay.dancher.DomainEvent
import com.github.unisay.dancher.dom._
import monix.reactive.Observable
import org.scalajs.dom.{Element, Node}
import scala.language.implicitConversions

object JsInterpreter extends ActionInterpreter {

  case class EffectActionEvent(effect: String) extends DomainEvent

  type Script = List[String]
  type Result = (Map[String, Int], Script)
  type InterpreterState[A] = State[Result, A]

  def interpret[R, M](model: M, action: ActionF[R]): R = interpretScript(model, action, Observable.empty)._1

  def interpretScript[R, M](model: M, action: ActionF[R],
                            domEvents: Observable[(String, DomEvent)] = Observable.empty): (R, String) = {
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
        implicit def nodeToA(node: Node): A = node.asInstanceOf[A]
        implicit def elementToA(element: Element): A = element.asInstanceOf[A]
        implicit def toA(aw: InterpreterState[_]): InterpreterState[A] = aw.asInstanceOf[InterpreterState[A]]

        action match {

          case NoAction =>
            noModification

          case Value(a: Any) =>
            result(a.asInstanceOf[A])

          case CreateElement(tagName) =>
            ???
/*            scriptWithCounterAndReturn(tagName) { variableName =>
              (s"var $variableName = document.createElement('$tagName')", variableName)
            }*/

          case CreateTextNode(text) =>
            ???
/*
            scriptWithCounterAndReturn("text") { variableName =>
              (s"var $variableName = document.createTextNode('$text')", variableName)
            }
*/

          case AppendChild(parent, child) =>
            scriptWithReturn(s"$parent.appendChild($child)", parent)


          case RemoveChild(parent, child) =>
            scriptWithReturn(s"$parent.removeChild($child)", parent)

          case ReplaceChild(parent, newChild, oldChild) =>
            scriptWithReturn(s"$parent.replaceChild($newChild, $oldChild)", parent)

          case SetAttribute(element, name, value) =>
            scriptWithReturn(s"""$element.setAttribute("$name", "$value")""", element)

          case HandleEvents(element, eventTypes) =>
            val result = domEvents
              .flatMap {
                case (el, domEvent) if el === element.tagName => Observable(Ior.Left(domEvent))
                case _ => Observable.empty
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
