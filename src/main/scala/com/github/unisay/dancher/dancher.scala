package com.github.unisay.dancher

import java.util.UUID

import com.github.unisay.dancher.DomAction.{AppendChild, SetAttribute, SetOnClick}

import scala.language.implicitConversions
import scalaz.{Free, Node ⇒ _}
import DomAction._

trait DomEvent
trait DomMouseEvent extends DomEvent

trait DomNode {
  def appendChild(child: DomNode): DomActionF[DomNode] = AppendChild(this, child, this)
  def removeChild(child: DomNode): DomActionF[DomNode] = RemoveChild(this, child, this)
  def getParent: DomActionF[DomNode] = GetParent(this, identity)
}

trait DomElement extends DomNode {
  def setAttribute(name: String, value: String): DomActionF[DomElement] = SetAttribute(this, name, value, this)
  def setClass(cssClass: String): DomActionF[DomElement] = setAttribute("class", cssClass)
  def onClick[A, E](handler: DomMouseEventHandler[E]): DomActionF[DomElement] = SetOnClick(this, handler, this)
}

trait DomNodeList

abstract class Model(val domId: DomId) {

  /** Returns current DOM element */
  def element: DomActionF[DomElement] = getElementById(domId.value)

  /** Returns parent node */
  def parent: DomActionF[DomNode] =
    for {
      child ← element
      parent ← child.getParent
    } yield parent

  /** Creates model and returns its topmost DOM element (root) */
  def create: DomActionF[DomElement]

  /** Removes current model from it's parent DOM node and returns parent */
  def remove: DomActionF[DomNode] =
    for {
      child ← element
      parent ← child.getParent
      _ ← parent removeChild child
    } yield parent

  def replaceWith(that: Model): DomActionF[DomNode] =
    for {
      parent ← this.remove
      child ← that.create
      _ ← parent appendChild child
    } yield parent

}

trait ModelComparator {
  def diff(old: Model, updated: Model): DomActionF[_]
}

trait DomainEvent

sealed trait DomAction[+N]

case class DomId(value: String) extends AnyVal

trait Gen[A] {
  def generate: A
}

object Gen {
  implicit val DomIdGen: Gen[DomId] = new Gen[DomId] {
    def generate: DomId = DomId(UUID.randomUUID.toString)
  }
}

object DomAction {
  implicit def actionToFree[A](action: DomAction[A]): DomActionF[A] = Free.liftF(action)

  object NoAction extends DomAction[Unit]
  val noAction: DomActionF[Unit] = NoAction

  case class Log(text: String) extends DomAction[Unit]
  def log(message: String): DomActionF[Unit] = Log(message)

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
  case class RemoveChild[N](parent: DomNode, child: DomNode, next: N) extends DomAction[N]
  case class GetParent[N](node: DomNode, nodeToNext: DomNode ⇒ N) extends DomAction[N]
  case class SetOnClick[N, E](element: DomElement, handler: DomMouseEventHandler[E], next: N) extends DomAction[N]
}