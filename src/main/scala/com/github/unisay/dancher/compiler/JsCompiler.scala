package com.github.unisay.dancher.compiler

import cats.data.Writer
import cats.std.list._
import cats.syntax.writer._
import cats.~>
import com.github.unisay.dancher.dom._

import scala.language.implicitConversions

class JsCompiler {
  case class RawElement(element: String) extends DomElement
  case class RawNodeList(nodeList: String) extends DomNodeList
  case class RawMouseEvent(event: String) extends DomMouseEvent
  case class RawNode(node: String) extends DomNode

  type ActionWriter[A] = Writer[List[String], A]

  var counter = 0 // Mutable state ?

  val compiler: Action ~> ActionWriter = new (Action ~> ActionWriter) {
    def comment(comment: String): List[String] = List(s"/* $comment */")
    def variable(expression: String, prefix: String = "id"): (String, List[String]) = {
      val v = id(prefix)
      (v, s"var $v = $expression" :: Nil)
    }
    def id(prefix: String): String = {
      counter = counter + 1
      prefix + counter
    }

    override def apply[A](fa: Action[A]): ActionWriter[A] = {
      implicit def writerToA(aw: ActionWriter[_]): ActionWriter[A] =
        aw.asInstanceOf[ActionWriter[A]]

      fa match {

        case NoAction ⇒
          comment("No Action").tell

        case GetDocumentBody ⇒
          RawElement("b").writer("var b = document.body" :: Nil)

        case GetParent(RawNode(node)) ⇒
          RawNode(s"$node.parentNode").writer(comment(s"GetParent($node)"))

        case GetElementById(elementId) ⇒
          RawElement(s"document.getElementById('${elementId.value}')")
            .writer(comment(s"GetElementById(${elementId.value})"))

        case GetElementsByName(name) ⇒
          RawNodeList(s"document.getElementsByName('$name')")
            .writer(comment(s"GetElementsByName($name)"))

        case GetElementsByTagName(name) ⇒
          RawNodeList(s"document.getElementsByName('$name')")
            .writer(comment(s"GetElementsByTagName($name)"))

        case GetElementsByClassName(name) ⇒
          RawNodeList(s"document.getElementsByName('$name')")
            .writer(comment(s"GetElementsByClassName($name)"))

        case CreateElement(tagName) ⇒
          val (name, record) = variable(s"document.createElement('$tagName')", tagName)
          RawElement(name).writer(record)

        case CreateTextNode(text) ⇒
          val (name, record) = variable(s"document.createTextNode('$text')", "text")
          RawNode(name).writer(record)

        case AppendChild(rawParent@RawElement(parent), RawNode(child)) ⇒
          rawParent.writer(s"$parent.appendChild($child)" :: Nil)

        case AppendChild(rawParent@RawElement(parent), RawElement(child)) ⇒
          rawParent.writer(s"$parent.appendChild($child)" :: Nil)

        case RemoveChild(rawParent@RawElement(parent), RawNode(child)) ⇒
          rawParent.writer(s"$parent.removeChild($child)" :: Nil)

        case GetFirstChild(RawNode(node)) ⇒
          RawNode(s"$node.firstChild").writer(comment(s"GetFirstChild($node)"))

        case ReplaceChild(rawParent@RawElement(parent), RawNode(oldChild), RawNode(newChild)) ⇒
          rawParent.writer(s"$parent.replaceChild($oldChild, $newChild)" :: Nil)

        case SetAttribute(rawElement@RawElement(element), name, value) ⇒
          rawElement.writer(s"$element.setAttribute('$name', '$value')" :: Nil)

        case SetOnClick(rawElement@RawElement(element), handler) ⇒
          rawElement.writer(comment(s"SetOnClick($element)"))

        case it ⇒
          comment(s"ERROR: $it").tell

      }
    }
  }

  def compile[A](action: ActionF[A]): List[String] = action.foldMap(compiler).written
}
