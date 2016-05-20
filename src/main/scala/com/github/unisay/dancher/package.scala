package com.github.unisay

import org.scalajs.dom.raw._

import scala.language.{higherKinds, implicitConversions}
import scalaz.{Free, Node ⇒ _, _}, Id.Id

package object dancher {

  sealed trait Action[A]
  case class Log[A](text: String, a: A) extends Action[A]
  case class GetElementById[A](id: String, f: Element ⇒ A) extends Action[A]
  case class CreateElement[A](tagName: String, f: Element ⇒ A) extends Action[A]
  case class CreateTextNode[A](text: String, f: Text ⇒ A) extends Action[A]
  case class AppendChild[A](parent: Node, child: Node, a: A) extends Action[A]

  implicit val ActionFunctor: Functor[Action] = new Functor[Action] {
    override def map[A, B](action: Action[A])(f: A ⇒ B): Action[B] = action match {
      case Log(text, next) ⇒
        Log(text, f(next))
      case GetElementById(id, g) ⇒
        GetElementById(id, g andThen f)
      case CreateElement(tagName, g) ⇒
        CreateElement(tagName, g andThen f)
      case CreateTextNode(text, g) ⇒
        CreateTextNode(text, g andThen f)
      case AppendChild(parent, child, next) ⇒
        AppendChild(parent, child, f(next))
    }
  }

  type FAction[A] = Free[Action, A]

  trait Actions {
    implicit def actionToFree[A](action: Action[A]): Free[Action, A] = Free.liftF(action)

    def log(message: String) = Log(message, message)
    def getElementById(id: String) = GetElementById(id, identity)
    def createElement(tagName: String) = CreateElement(tagName, identity)
    def createTextNode(text: String) = CreateTextNode(text, identity)
    def appendChild(parent: Node, child: Node) = AppendChild(parent, child, child)
  }

  object Actions extends Actions

  trait Widget extends Actions {
    def create: FAction[Element]
  }
}
