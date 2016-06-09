package com.github.unisay.dancher.compiler

import cats.data.State
import cats.~>
import com.github.unisay.dancher.dom._

import scala.language.implicitConversions

object JsCompiler {

  case class RawElement(element: String) extends DomElement
  case class RawNodeList(nodeList: String) extends DomNodeList
  case class RawMouseEvent(event: String) extends DomMouseEvent
  case class RawNode(node: String) extends DomNode

  type Script = List[String]
  type CounterWithScript = (Int, Script)
  type CompilerState[A] = State[CounterWithScript, A]

  val compiler: Action ~> CompilerState = new (Action ~> CompilerState) {

    def result[A](a: A): CompilerState[A] = State.pure(a)

    def commentWithReturn[A](comment: String, a: A): CompilerState[A] =
      scriptWithReturn(s"/* $comment */", a)

    def comment(c: String): CompilerState[Unit] =
      State.modify { case (counter, script) ⇒
        (counter, s"/* $c */" :: script)
    }

    def scriptWithReturn[A](line: String, a: A): CompilerState[A] =
      State.modify[CounterWithScript]{ case (counter, script) ⇒ (counter, line :: script) }.map(_ ⇒ a)

    def scriptWithCounterAndReturn[A](f: Int ⇒ (String, A)): CompilerState[A] =
      State { case (counter, script) ⇒
        val (line, a) = f(counter)
        ((counter + 1, line :: script), a)
      }

    override def apply[A](action: Action[A]): CompilerState[A] = {
      implicit def toA(aw: CompilerState[_]): CompilerState[A] = aw.asInstanceOf[CompilerState[A]]

      action match {

        case NoAction ⇒
          comment("No Action")

        case GetDocumentBody ⇒
          scriptWithReturn("var b = document.body", RawElement("b"))

        case GetParent(RawNode(node)) ⇒
          result(RawNode(s"$node.parentNode"))

        case GetElementById(elementId) ⇒
          result(RawElement(s"document.getElementById('${elementId.value}')"))

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
          scriptWithCounterAndReturn[RawElement] { counter ⇒
            val variableName = s"text$counter"
            (s"var $variableName = document.createTextNode('$text')", RawElement(variableName))
          }

        case AppendChild(rawParent@RawElement(parent), RawNode(child)) ⇒
          scriptWithReturn(s"$parent.appendChild($child)", rawParent)

        case AppendChild(rawParent@RawElement(parent), RawElement(child)) ⇒
          scriptWithReturn(s"$parent.appendChild($child)", rawParent)

        case RemoveChild(rawParent@RawElement(parent), RawNode(child)) ⇒
          scriptWithReturn(s"$parent.removeChild($child)", rawParent)

        case GetFirstChild(RawNode(node)) ⇒
          result(RawNode(s"$node.firstChild"))

        case ReplaceChild(rawParent@RawElement(parent), RawNode(oldChild), RawNode(newChild)) ⇒
          scriptWithReturn(s"$parent.replaceChild($oldChild, $newChild)", rawParent)

        case SetAttribute(rawElement@RawElement(element), name, value) ⇒
          scriptWithReturn(s"$element.setAttribute('$name', '$value')", rawElement)

        case SetOnClick(rawElement@RawElement(element), handler) ⇒
          commentWithReturn(s"SetOnClick($element)", rawElement)

        case it ⇒
          comment(s"ERROR: $it")

      }
    }
  }

  def compile[A](action: ActionF[A]): Script = {
    val (_, reverseScript) = action.foldMap(compiler).runS((1, Nil)).value
    reverseScript.reverse
  }

}
