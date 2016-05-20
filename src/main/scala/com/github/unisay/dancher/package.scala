package com.github.unisay

import org.scalajs.dom.document._
import org.scalajs.dom.raw._

import scala.language.{higherKinds, implicitConversions}
import scalaz.{Free, _}, Id.Id

package object dancher {

  sealed trait Action[A]
  case class Log[A](text: String, a: A) extends Action[A]
  case class GetElementById[A](id: String, f: Element ⇒ A) extends Action[A]
  case class CreateElement[A](tagName: String, f: Element ⇒ A) extends Action[A]
  case class AppendChild[A](parent: Element, child: Element, a: A) extends Action[A]

  implicit val ActionFunctor: Functor[Action] = new Functor[Action] {
    override def map[A, B](action: Action[A])(f: A ⇒ B): Action[B] = action match {
      case Log(text, next) ⇒
        Log(text, f(next))
      case GetElementById(id, g) ⇒
        GetElementById(id, g andThen f)
      case CreateElement(tagName, g) ⇒
        CreateElement(tagName, g andThen f)
      case AppendChild(parent, child, next) ⇒
        AppendChild(parent, child, f(next))
    }
  }

  type DomAction[A] = Free[Action, A]

  object DomActions {
    def log(message: String): DomAction[String] = Free.liftF(Log(message, message))
    def getElementById(id: String): DomAction[Element] = Free.liftF(GetElementById(id, identity))
    def createElement(tagName: String): DomAction[Element] = Free.liftF(CreateElement(tagName, identity))
    def appendChild(parent: Element, child: Element): DomAction[Element] = Free.liftF(AppendChild(parent, child, child))
  }

  sealed trait Widget {
    def create: DomAction[Element]
  }

  case class Label(text: String) extends Widget {
    import DomActions._

    override def create: DomAction[Unit] = for {
      _ ← log("Getting parent element")
      parent ← getElementById("parent")
      _ ← log("Creating DIV")
      child ← createElement("div")
      _ ← log("Appending DIV to parent")
      element ← appendChild(parent, child)
      _ ← log("Label created")
    } yield element
  }

  class DomActionRunner {
    private val exe: DomAction ~> Id = new (DomAction ~> Id) {
      def apply[A](fa: DomAction[A]): Id[A] = fa match {
        case // TODO
      }
    }

    def run[A](domAction: DomAction[A]): A = domAction.runM(exe.apply[DomAction[A]])
  }

}
