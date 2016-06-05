package com.github.unisay.dancher

import cats.data.State
import cats.~>
import com.github.unisay.dancher.dom._

class TestCompiler {
  case class RawElement(element: String) extends DomElement
  case class RawNodeList(nodeList: String) extends DomNodeList
  case class RawMouseEvent(event: String) extends DomMouseEvent
  case class RawNode(node: String) extends DomNode

  type ActionState[A] = State[List[String], A]

  val compiler: Action ~> ActionState = new (Action ~> ActionState) {

    override def apply[A](fa: Action[A]): ActionState[A] = {

      fa match {

        case NoAction ⇒
          State.set[List[String]]("NoAction" :: Nil).asInstanceOf[ActionState[A]]

        case GetDocumentBody ⇒
          State.modify[List[String]]("document.body" :: _).asInstanceOf[ActionState[A]]
/*

        case GetParent(RawNode(node)) ⇒
          RawNode(s"$node.parentNode")

        case GetElementById(elementId) ⇒
          RawElement(s"getElementById(${elementId.value})")

        case GetElementsByName(name) ⇒
          RawNodeList(s"document.getElementsByName($name)")

        case GetElementsByTagName(name) ⇒
          RawNodeList(s"document.getElementsByName($name)")

        case GetElementsByClassName(name) ⇒
          RawNodeList(s"document.getElementsByName($name)")

        case CreateElement(tagName) ⇒
          RawElement(s"document.createElement($tagName)")

        case CreateTextNode(text) ⇒
          RawNode(s"document.createTextNode($text)")

        case AppendChild(RawElement(parent), RawNode(child)) ⇒
          s"$parent.appendChild($child)" :: Nil

        case AppendChild(RawElement(parent), RawElement(child)) ⇒
          s"$parent.appendChild($child)" :: Nil

        case RemoveChild(RawElement(parent), RawNode(child)) ⇒
          s"$parent.removeChild($child)" :: Nil

        case GetFirstChild(RawNode(node)) ⇒
          RawNode(s"$node.firstChild")

        case ReplaceChild(rawParent@RawElement(parent), RawNode(oldChild), RawNode(newChild)) ⇒
          s"$parent.replaceChild($oldChild, $newChild)" :: Nil

        case SetAttribute(RawElement(element), name, value) ⇒
          s"$element.setAttribute($name, $value)" :: Nil

        case SetOnClick(RawElement(element), handler) ⇒
          s"$element.addEventListener('click', $handler)" :: Nil
*/

        case it ⇒
          State.set[List[String]](s"ERROR: $it" :: Nil).asInstanceOf[ActionState[A]]

      }
    }
  }

  def compile[A](action: ActionF[A]): ActionState[A] = action.foldMap(compiler)
}
