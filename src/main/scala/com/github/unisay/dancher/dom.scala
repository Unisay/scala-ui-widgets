package com.github.unisay.dancher

import cats.data.Ior
import cats.free.Free
import monix.reactive.Observable

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

  type DomEventHandler[M] = DomEvent => Observable[Ior[M, DomainEvent]]

  trait DomNode {
    def getParent = dom.getParentNode(this)
    def getFirstChild = dom.getFirstChild(this)
    def removeChild(child: DomNode) = dom.removeChild(this, child)
    def replaceChild(newChild: DomNode, oldChild: DomNode) = dom.replaceChild(this, newChild, oldChild)
  }

  trait DomElement extends DomNode {
    def setId(id: DomId) = dom.setId(id)(this)
    def appendChild[C <: DomNode](child: C) = dom.appendChild(this, child)
    def appendText(text: String) = dom.createTextNode(text).flatMap(this.appendChild)
    def setAttribute(name: String, value: String) = dom.setAttribute(name, value)(this)
    def setClass(cssClass: String) = dom.setClass(cssClass)(this)
    def hide = setClass("d-hidden")
    def setClasses(cssClasses: List[String]) = setClass(cssClasses.map(_.trim).filter(_.nonEmpty).mkString(" "))
    def onClick[M](handler: DomEventHandler[M]) = dom.setOnClick(this, handler)
  }

  case class DomBinding[M](element: DomElement, events: Observable[M Ior DomainEvent] = Observable.empty)

  type ActionF[A] = Free[Action, A]

  object NoAction extends Action[Nothing]
  def noAction[R]: ActionF[R] = NoAction

  case class Value[A](a: A) extends Action[A]
  def value[A](a: A): ActionF[A] = Value(a)

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

  case class AppendChild[C <: DomNode](parent: DomElement, child: C) extends Action[C]
  def appendChild[C <: DomNode](parent: DomElement, child: C): ActionF[C] = AppendChild(parent, child)

  case class RemoveChild(parent: DomNode, child: DomNode) extends Action[DomNode]
  def removeChild(parent: DomNode, child: DomNode): ActionF[DomNode] = RemoveChild(parent, child)

  case class ReplaceChild(parent: DomNode, newChild: DomNode, oldChild: DomNode) extends Action[DomNode]
  def replaceChild(parent: DomNode, newChild: DomNode, oldChild: DomNode): ActionF[DomNode] =
    ReplaceChild(parent, newChild, oldChild)

  case class SetAttribute(element: DomElement, name: String, value: String) extends Action[DomElement]
  def setAttribute(name: String, value: String)(elem: DomElement): ActionF[DomElement] = SetAttribute(elem, name, value)
  def setId(id: DomId)(element: DomElement) = setAttribute("id", id.value)(element)
  def setClass(cssClass: String)(element: DomElement) = setAttribute("class", cssClass)(element)

  case class SetOnClick[M](element: DomElement, handler: DomEventHandler[M])
    extends Action[Observable[M Ior DomainEvent]]
  def setOnClick[M](element: DomElement, handler: DomEventHandler[M]): ActionF[Observable[M Ior DomainEvent]] =
    SetOnClick(element, handler)

}
