package com.github.unisay.dancher

import cats.data.Writer
import cats.std.string._
import cats.syntax.writer._
import cats.~>
import com.github.unisay.dancher.dom._

import scala.language.implicitConversions

class JsCompiler {
  case class RawElement(element: String) extends DomElement
  case class RawNodeList(nodeList: String) extends DomNodeList
  case class RawMouseEvent(event: String) extends DomMouseEvent
  case class RawNode(node: String) extends DomNode

  type ActionWriter[A] = Writer[String, A]

  val compiler: Action ~> ActionWriter = new (Action ~> ActionWriter) {

    override def apply[A](fa: Action[A]): ActionWriter[A] = {
      implicit def writerToA(aw: ActionWriter[_]): ActionWriter[A] =
        aw.asInstanceOf[ActionWriter[A]]

      fa match {

        case NoAction ⇒
          "NoAction".tell

        case GetDocumentBody ⇒
          RawElement("document.body").writer("GetDocumentBody")

        case GetParent(RawNode(node)) ⇒
          RawNode(s"$node.parentNode").writer(s"GetParent($node)")

        case GetElementById(elementId) ⇒
          RawElement(s"document.getElementById(${elementId.value})").writer(s"GetElementById(${elementId.value})")

        case GetElementsByName(name) ⇒
          RawNodeList(s"document.getElementsByName($name)").writer(s"GetElementsByName($name)")

        case GetElementsByTagName(name) ⇒
          RawNodeList(s"document.getElementsByName($name)").writer(s"GetElementsByTagName($name)")

        case GetElementsByClassName(name) ⇒
          RawNodeList(s"document.getElementsByName($name)").writer(s"GetElementsByClassName($name)")

        case CreateElement(tagName) ⇒
          RawElement(s"document.createElement($tagName)").writer(s"CreateElement(tagName)")

        case CreateTextNode(text) ⇒
          RawNode(s"document.createTextNode($text)").writer(s"CreateTextNode(text)")

        case AppendChild(rawParent@RawElement(parent), RawNode(child)) ⇒
          rawParent.writer(s"$parent.appendChild($child)")

        case AppendChild(rawParent@RawElement(parent), RawElement(child)) ⇒
          rawParent.writer(s"$parent.appendChild($child)")

        case RemoveChild(rawParent@RawElement(parent), RawNode(child)) ⇒
          rawParent.writer(s"$parent.removeChild($child)")

        case GetFirstChild(RawNode(node)) ⇒
          RawNode(s"$node.firstChild").writer(s"GetFirstChild($node)")

        case ReplaceChild(rawParent@RawElement(parent), RawNode(oldChild), RawNode(newChild)) ⇒
          rawParent.writer(s"$parent.replaceChild($oldChild, $newChild)")

        case SetAttribute(rawElement@RawElement(element), name, value) ⇒
          rawElement.writer(s"$element.setAttribute($name, $value)")

        case SetOnClick(rawElement@RawElement(element), handler) ⇒
          rawElement.writer(s"$element.addEventListener('click', $handler)")

        case it ⇒
          s"ERROR: $it".tell

      }
    }
  }

  def compile[A](action: ActionF[A]): ActionWriter[A] = action.foldMap(compiler)
}
