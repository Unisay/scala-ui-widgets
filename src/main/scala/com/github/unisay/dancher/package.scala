package com.github.unisay

import com.github.unisay.dancher.DomAction._

import scala.language.{higherKinds, implicitConversions}
import scalaz.{Free, Node ⇒ _}

package object dancher {

  trait DomNode {
    def appendChild(child: DomNode): DomActionF[DomNode] = AppendChild(this, child, this)
  }

  trait DomEvent
  trait DomMouseEvent extends DomEvent

  type DomEventHandler[E <: DomEvent, C] = E ⇒ C // TODO: C[E] ?
  type DomMouseEventHandler[C] = DomEventHandler[DomMouseEvent, C]

  trait DomElement extends DomNode {
    def setAttribute(name: String, value: String): DomActionF[DomElement] = SetAttribute(this, name, value, this)
    def setClass(cssClass: String): DomActionF[DomElement] = setAttribute("class", cssClass)
    def onClick[A, E](handler: DomMouseEventHandler[E]): DomActionF[DomElement] = SetOnClick(this, handler, this)
  }

  trait DomNodeList

  sealed trait DomAction[N]

  type DomActionF[A] = Free[DomAction, A]

  trait Model {
    def create: DomActionF[DomElement]
  }

  trait ModelComparator {
    def diff(old: Model, updated: Model): DomActionF[Unit]
  }

  trait Widget extends Model

  trait DomainEvent
  type DomainEventHandler = PartialFunction[(DomainEvent, Model), Model]

  object DomAction {
    implicit def actionToFree[A](action: DomAction[A]): DomActionF[A] = Free.liftF(action)

    case class Log[N](text: String, next: N) extends DomAction[N]
    def log(message: String): DomActionF[Unit] = Log(message, ())

    case class GetDocumentBody[N](elementToNext: DomElement ⇒ N) extends DomAction[N]
    def getDocumentBody: DomActionF[DomElement] = GetDocumentBody(identity)

    case class GetElementById[N](id: String, elementToNext: DomElement ⇒ N) extends DomAction[N]
    def getElementById(id: String): DomActionF[DomElement] = GetElementById(id, identity)

    case class GetElementsByName[N](Name: String, nodeListToNext: DomNodeList ⇒ N) extends DomAction[N]
    def getElementsByName(Name: String): DomActionF[DomNodeList] = GetElementsByName(Name, identity)

    case class GetElementsByTagName[N](tagName: String, nodeListToNext: DomNodeList ⇒ N) extends DomAction[N]
    def getElementsByTagName(tagName: String): DomActionF[DomNodeList] = GetElementsByTagName(tagName, identity)

    case class GetElementsByClassName[N](className: String, nodeListToNext: DomNodeList ⇒ N) extends DomAction[N]
    def getElementsByClassName(className: String): DomActionF[DomNodeList] = GetElementsByClassName(className, identity)

    case class CreateElement[N](tagName: String, elementToNext: DomElement ⇒ N) extends DomAction[N]
    def createElement(tagName: String): DomActionF[DomElement] = CreateElement(tagName, identity)

    case class CreateTextNode[N](text: String, nodeToNext: DomNode ⇒ N) extends DomAction[N]
    def createTextNode(text: String): DomActionF[DomNode] = CreateTextNode(text, identity)

    case class SetAttribute[N](element: DomElement, name: String, value: String, next: N) extends DomAction[N]
    case class AppendChild[N](parent: DomNode, child: DomNode, next: N) extends DomAction[N]

    case class SetOnClick[N, E](element: DomElement, handler: DomMouseEventHandler[E], next: N) extends DomAction[N]
  }

}
