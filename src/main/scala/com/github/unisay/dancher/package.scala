package com.github.unisay

import com.github.unisay.dancher.Action._

import scala.language.{higherKinds, implicitConversions}
import scalaz.{Free, Node ⇒ _}

package object dancher {

  trait DomNode {
    def appendChild(child: DomNode): ActionF[DomNode] = AppendChild(this, child, this)
  }

  trait DomMouseEvent
  type MouseEventHandler = DomMouseEvent ⇒ Unit

  trait DomElement extends DomNode {
    def setAttribute(name: String, value: String): ActionF[DomElement] = SetAttribute(this, name, value, this)
    def setClass(cssClass: String): ActionF[DomElement] = setAttribute("class", cssClass)
    def onClick[A](handler: MouseEventHandler): ActionF[DomElement] = SetOnClick(this, handler, this)
  }

  trait DomNodeList

  sealed trait Action[Next]

  type ActionF[A] = Free[Action, A]

  object Action {
    implicit def actionToFree[A](action: Action[A]): ActionF[A] = Free.liftF(action)

    case class Log[N](text: String, next: N) extends Action[N]
    def log(message: String) = Log(message, message)

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

    case class SetOnClick[N](element: DomElement, handler: MouseEventHandler, next: N) extends Action[N]
  }

}
