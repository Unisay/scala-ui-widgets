package com.github.unisay.dancher

import cats.free.Free

import scala.language.implicitConversions

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
    def replaceChild(newChild: DomNode, oldChild: DomNode) = dom.replaceChild(this, newChild, oldChild)
  }

  trait DomElement extends DomNode {
    def setId(id: DomId) = dom.setId(id)(this)
    def setAttribute(name: String, value: String) = dom.setAttribute(name, value)(this)
    def setClass(cssClass: String) = dom.setClass(cssClass)(this)
    def onClick(handler: DomEventHandler) = dom.setOnClick(this, handler)
  }


  type ActionF[A] = Free[Action, A]
  type WidgetAction = ActionF[DomElement]

  object NoAction extends Action[Unit]
  val noAction: ActionF[Unit] = NoAction

  case class Log(text: String) extends Action[Unit]
  def log(message: String): ActionF[Unit] =
    Log(message)

  object GetDocumentBody extends Action[DomElement]
  def getDocumentBody: ActionF[DomElement] = GetDocumentBody

  case class GetElementById(id: DomId) extends Action[DomElement]
  def getElementById(id: DomId): ActionF[DomElement] = GetElementById(id)

  case class GetElementsByName(Name: String) extends Action[DomNodeList]
  def getElementsByName(Name: String): ActionF[DomNodeList] = GetElementsByName(Name)

  case class GetElementsByTagName(tagName: String) extends Action[DomNodeList]
  def getElementsByTagName(tagName: String): ActionF[DomNodeList] = GetElementsByTagName(tagName)

  case class GetElementsByClassName(className: String) extends Action[DomNodeList]
  def getElementsByClassName(className: String): ActionF[DomNodeList] = GetElementsByClassName(className)

  case class CreateElement(tagName: String) extends Action[DomElement]
  def createElement(tagName: String): ActionF[DomElement] = CreateElement(tagName)

  case class CreateTextNode(text: String) extends Action[DomNode]
  def createTextNode(text: String): ActionF[DomNode] = CreateTextNode(text)

  case class GetParent(node: DomNode) extends Action[DomNode]
  def getParentNode(node: DomNode): ActionF[DomNode] = GetParent(node)

  case class GetFirstChild(node: DomNode) extends Action[DomNode]
  def getFirstChild(node: DomNode): ActionF[DomNode] = GetFirstChild(node)

  case class AppendChild(parent: DomNode, child: DomNode) extends Action[DomNode]
  def appendChild(parent: DomNode, child: DomNode): ActionF[DomNode] = AppendChild(parent, child)

  case class RemoveChild(parent: DomNode, child: DomNode) extends Action[DomNode]
  def removeChild(parent: DomNode, child: DomNode): ActionF[DomNode] = RemoveChild(parent, child)

  case class ReplaceChild(parent: DomNode, newChild: DomNode, oldChild: DomNode) extends Action[DomNode]
  def replaceChild(parent: DomNode, newChild: DomNode, oldChild: DomNode): ActionF[DomNode] =
    ReplaceChild(parent, newChild, oldChild)

  case class SetAttribute(element: DomElement, name: String, value: String) extends Action[DomElement]
  def setAttribute(name: String, value: String)(elem: DomElement): ActionF[DomElement] = SetAttribute(elem, name, value)
  def setId(id: DomId)(element: DomElement) = setAttribute("id", id.value)(element)
  def setClass(cssClass: String)(element: DomElement) = setAttribute("class", cssClass)(element)

  case class SetOnClick[N, E](element: DomElement, event: DomEventHandler) extends Action[DomElement]
  def setOnClick(element: DomElement, handler: DomEventHandler): ActionF[DomElement] = SetOnClick(element, handler)

}
