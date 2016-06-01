package com.github.unisay.dancher

import scala.language.implicitConversions
import scalaz.Free

object dom extends gen {

  implicit def actionToFree[A](action: Action[A]): ActionF[A] = Free.liftF(action)

  sealed trait Action[+N]

  object DomId {
    implicit def symbolToDomId(symbol: Symbol): DomId = DomId(symbol.name)
  }

  case class DomId(value: String) extends AnyVal

  trait DomEvent
  trait DomMouseEvent extends DomEvent
  trait DomNodeList

  trait DomNode {
    def getParent = dom.getParentNode(this)
    def getFirstChild = dom.getFirstChild(this)
    def appendChild(child: DomNode) = dom.appendChild(this, child)
    def removeChild(child: DomNode) = dom.removeChild(this, child)
    def replaceChild(oldChild: DomNode, newChild: DomNode) = dom.replaceChild(this, oldChild, newChild)
  }

  trait DomElement extends DomNode {
    def setId(id: DomId) = dom.setId(id)(this)
    def setAttribute(name: String, value: String) = dom.setAttribute(name, value)(this)
    def setClass(cssClass: String) = dom.setClass(cssClass)(this)
    def onClick(handler: DomEventHandler) = dom.setOnClick(this, handler)
  }


  type ActionF[A] = Free[Action, A]

  object NoAction extends Action[Unit]
  val noAction: ActionF[Unit] = NoAction

  case class Log(text: String) extends Action[Unit]
  def log(message: String): ActionF[Unit] =
    Log(message)

  case class GetDocumentBody[N](elementToNext: DomElement ⇒ N) extends Action[N]
  def getDocumentBody: ActionF[DomElement] =
    GetDocumentBody(identity)

  case class GetElementById[N](id: DomId, elementToNext: DomElement ⇒ N) extends Action[N]
  def getElementById(id: DomId): ActionF[DomElement] =
    GetElementById(id, identity)

  case class GetElementsByName[N](Name: String, nodeListToNext: DomNodeList ⇒ N) extends Action[N]
  def getElementsByName(Name: String): ActionF[DomNodeList] =
    GetElementsByName(Name, identity)

  case class GetElementsByTagName[N](tagName: String, nodeListToNext: DomNodeList ⇒ N) extends Action[N]
  def getElementsByTagName(tagName: String): ActionF[DomNodeList] =
    GetElementsByTagName(tagName, identity)

  case class GetElementsByClassName[N](className: String, nodeListToNext: DomNodeList ⇒ N) extends Action[N]
  def getElementsByClassName(className: String): ActionF[DomNodeList] =
    GetElementsByClassName(className, identity)

  case class CreateElement[N](tagName: String, elementToNext: DomElement ⇒ N) extends Action[N]
  def createElement(tagName: String): ActionF[DomElement] =
    CreateElement(tagName, identity)

  case class CreateTextNode[N](text: String, nodeToNext: DomNode ⇒ N) extends Action[N]
  def createTextNode(text: String): ActionF[DomNode] =
    CreateTextNode(text, identity)

  case class GetParent[N](node: DomNode, nodeToNext: DomNode ⇒ N) extends Action[N]
  def getParentNode(node: DomNode): ActionF[DomNode] =
    GetParent(node, identity)

  case class GetFirstChild[N](node: DomNode, nodeToNext: DomNode ⇒ N) extends Action[N]
  def getFirstChild(node: DomNode): ActionF[DomNode] =
    GetFirstChild(node, identity)

  case class AppendChild[N](parent: DomNode, child: DomNode, next: N) extends Action[N]
  def appendChild(parent: DomNode, child: DomNode): ActionF[DomNode] =
    AppendChild(parent, child, parent)

  case class RemoveChild[N](parent: DomNode, child: DomNode, next: N) extends Action[N]
  def removeChild(parent: DomNode, child: DomNode): ActionF[DomNode] =
    RemoveChild(parent, child, parent)

  case class ReplaceChild[N](parent: DomNode, oldChild: DomNode, newChild: DomNode, next: N) extends Action[N]
  def replaceChild(parent: DomNode, oldChild: DomNode, newChild: DomNode): ActionF[DomNode] =
    ReplaceChild(parent, oldChild, newChild, oldChild)

  case class SetAttribute[N](element: DomElement, name: String, value: String, next: N) extends Action[N]
  def setAttribute(name: String, value: String)(element: DomElement): ActionF[DomElement] =
    SetAttribute(element, name, value, element)
  def setId(id: DomId)(element: DomElement) = setAttribute("id", id.value)(element)
  def setClass(cssClass: String)(element: DomElement) = setAttribute("class", cssClass)(element)

  case class SetOnClick[N, E](element: DomElement, event: DomEventHandler, next: N) extends Action[N]
  def setOnClick(element: DomElement, handler: DomEventHandler): ActionF[DomElement] =
    SetOnClick(element, handler, element)

}
