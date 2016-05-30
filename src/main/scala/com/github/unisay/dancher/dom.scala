package com.github.unisay.dancher

import scala.language.implicitConversions
import scalaz.Free

object dom extends gen {

  implicit def actionToFree[A](action: Action[A]): ActionF[A] = Free.liftF(action)

  sealed trait Action[+N]

  case class DomId(value: String) extends AnyVal

  trait DomEvent
  trait DomMouseEvent extends DomEvent
  trait DomNodeList

  trait DomNode {

    def appendChild(child: DomNode): ActionF[DomNode] =
      AppendChild(this, child, this)

    def removeChild(child: DomNode): ActionF[DomNode] =
      RemoveChild(this, child, this)

    def replaceChild(oldChild: DomNode, newChild: DomNode): ActionF[DomNode] =
      ReplaceChild(this, oldChild, newChild, oldChild)

    def getParent: ActionF[DomNode] =
      GetParent(this, identity)
  }

  trait DomElement extends DomNode {

    def setAttribute(name: String, value: String): ActionF[DomElement] =
      SetAttribute(this, name, value, this)

    def setClass(cssClass: String): ActionF[DomElement] =
      setAttribute("class", cssClass)

    def onClick[A, E](handler: DomMouseEventHandler[E]): ActionF[DomElement] =
      SetOnClick(this, handler, this)
  }

  type DomEventHandler[E <: DomEvent, C] = E ⇒ C
  type DomMouseEventHandler[C] = DomEventHandler[DomMouseEvent, C]
  type ActionF[A] = Free[Action, A]

  object NoAction extends Action[Unit]
  val noAction: ActionF[Unit] = NoAction

  case class Log(text: String) extends Action[Unit]
  def log(message: String): ActionF[Unit] = Log(message)

  case class GetDocumentBody[N](elementToNext: DomElement ⇒ N) extends Action[N]
  def getDocumentBody: ActionF[DomElement] = GetDocumentBody(identity)

  case class GetElementById[N](id: String, elementToNext: DomElement ⇒ N) extends Action[N]
  def getElementById(id: String): ActionF[DomElement] = GetElementById(id, identity)

  case class GetElementsByName[N](Name: String, nodeListToNext: DomNodeList ⇒ N) extends Action[N]
  def getElementsByName(Name: String): ActionF[DomNodeList] = GetElementsByName(Name, identity)

  case class GetElementsByTagName[N](tagName: String, nodeListToNext: DomNodeList ⇒ N) extends Action[N]
  def getElementsByTagName(tagName: String): ActionF[DomNodeList] = GetElementsByTagName(tagName, identity)

  case class GetElementsByClassName[N](className: String, nodeListToNext: DomNodeList ⇒ N) extends Action[N]
  def getElementsByClassName(className: String): ActionF[DomNodeList] = GetElementsByClassName(className, identity)

  case class CreateElement[N](tagName: String, elementToNext: DomElement ⇒ N) extends Action[N]
  def createElement(tagName: String): ActionF[DomElement] = CreateElement(tagName, identity)

  case class CreateTextNode[N](text: String, nodeToNext: DomNode ⇒ N) extends Action[N]
  def createTextNode(text: String): ActionF[DomNode] = CreateTextNode(text, identity)

  case class SetAttribute[N](element: DomElement, name: String, value: String, next: N) extends Action[N]
  case class AppendChild[N](parent: DomNode, child: DomNode, next: N) extends Action[N]
  case class RemoveChild[N](parent: DomNode, child: DomNode, next: N) extends Action[N]
  case class ReplaceChild[N](parent: DomNode, oldChild: DomNode, newChild: DomNode, next: N) extends Action[N]
  case class GetParent[N](node: DomNode, nodeToNext: DomNode ⇒ N) extends Action[N]
  case class SetOnClick[N, E](element: DomElement, handler: DomMouseEventHandler[E], next: N) extends Action[N]
}