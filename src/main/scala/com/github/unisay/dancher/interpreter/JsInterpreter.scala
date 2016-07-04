package com.github.unisay.dancher.interpreter

import cats.data.State
import cats.~>
import com.github.unisay.dancher.dom._
import monix.reactive.Observable

import scala.language.implicitConversions

object JsInterpreter {

  case class RawElement(element: String) extends DomElement
  case class RawNodeList(nodeList: String) extends DomNodeList
  case class RawMouseEvent(event: String) extends DomMouseEvent
  case class RawNode(node: String) extends DomNode

  type Script = List[String]
  type CounterWithScript = (Int, Script)
  type InterpreterState[A] = State[CounterWithScript, A]

  val interpreter: Action ~> InterpreterState = new (Action ~> InterpreterState) {

    def result[A](a: A): InterpreterState[A] = State.pure(a)

    def commentWithReturn[A](comment: String, a: A): InterpreterState[A] =
      scriptWithReturn(s"/* $comment */", a)

    def comment(c: String): InterpreterState[Unit] =
      State.modify { case (counter, script) ⇒
        (counter, s"/* $c */" :: script)
    }

    def scriptWithReturn[A](line: String, a: A): InterpreterState[A] =
      State.modify[CounterWithScript]{ case (counter, script) ⇒ (counter, line :: script) }.map(_ ⇒ a)

    def scriptWithCounterAndReturn[A](f: Int ⇒ (String, A)): InterpreterState[A] =
      State { case (counter, script) ⇒
        val (line, a) = f(counter)
        ((counter + 1, line :: script), a)
      }

    override def apply[A](action: Action[A]): InterpreterState[A] = {
      implicit def toA(aw: InterpreterState[_]): InterpreterState[A] = aw.asInstanceOf[InterpreterState[A]]

      action match {

        case NoAction ⇒
          comment("No Action")

        case GetDocumentBody ⇒
          scriptWithReturn("var b = document.body", RawElement("b"))

        case GetParent(RawNode(node)) ⇒
          result(RawNode(s"$node.parentNode"))

        case GetParent(RawElement(node)) ⇒
          result(RawNode(s"$node.parentNode"))

        case GetElementById(elementId) ⇒
          scriptWithCounterAndReturn[RawElement] { counter ⇒
            val variableName = "element" + counter
            (s"var $variableName = document.getElementById('${elementId.value}')", RawElement(variableName))
          }

        case GetElementsByName(name) ⇒
          result(RawNodeList(s"document.getElementsByName('$name')"))

        case GetElementsByTagName(name) ⇒
          result(RawNodeList(s"document.getElementsByName('$name')"))

        case GetElementsByClassName(name) ⇒
          result(RawNodeList(s"document.getElementsByName('$name')"))

        case CreateElement(tagName) ⇒
          scriptWithCounterAndReturn[RawElement] { counter ⇒
            val variableName = tagName + counter
            (s"var $variableName = document.createElement('$tagName')", RawElement(variableName))
          }

        case CreateTextNode(text) ⇒
          scriptWithCounterAndReturn[RawNode] { counter ⇒
            val variableName = s"text$counter"
            (s"var $variableName = document.createTextNode('$text')", RawNode(variableName))
          }

        case AppendChild(rawParent@RawElement(parent), RawNode(child)) ⇒
          scriptWithReturn(s"$parent.appendChild($child)", rawParent)

        case AppendChild(rawParent@RawElement(parent), RawElement(child)) ⇒
          scriptWithReturn(s"$parent.appendChild($child)", rawParent)


        case RemoveChild(rawParent@RawElement(parent), RawNode(child)) ⇒
          scriptWithReturn(s"$parent.removeChild($child)", rawParent)

        case RemoveChild(rawParent@RawNode(parent), RawElement(child)) ⇒
          scriptWithReturn(s"$parent.removeChild($child)", rawParent)

        case GetFirstChild(RawNode(node)) ⇒
          result(RawNode(s"$node.firstChild"))

        case GetFirstChild(RawElement(node)) ⇒
          result(RawNode(s"$node.firstChild"))

        case ReplaceChild(rawParent@RawElement(parent), RawNode(newChild), RawNode(oldChild)) ⇒
          scriptWithReturn(s"$parent.replaceChild($newChild, $oldChild)", rawParent)

        case SetAttribute(rawElement@RawElement(element), name, value) ⇒
          scriptWithReturn(s"$element.setAttribute('$name', '$value')", rawElement)

        case SetOnClick(rawElement@RawElement(element), _) ⇒
          commentWithReturn(s"SetOnClick($element)", Observable.empty)

        case it ⇒
          sys.error(s"Can't compile to JS: $it")

      }
    }
  }

  def compile[A](action: ActionF[A]): Script = {
    val (_, reverseScript) = action.foldMap(interpreter).runS((1, Nil)).value
    reverseScript.reverse
  }

}
