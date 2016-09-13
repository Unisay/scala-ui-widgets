package com.github.unisay.dancher

import java.util.UUID

import cats.data.{Ior, NonEmptyList}
import cats.free.Free
import com.github.unisay.dancher.interpreter.ActionInterpreter
import com.github.unisay.dancher.widget.EffectAction
import monix.reactive.Observable
import org.scalajs.dom.{Element, Node}
import scala.language.implicitConversions

object dom {

  implicit def actionToFree[A](action: Action[A]): ActionF[A] = Free.liftF(action)

  sealed trait Action[+N]

  object DomId {
    def unique: DomId = DomId(UUID.randomUUID().toString)
  }

  case class DomId(value: String) extends AnyVal

  sealed trait DomEventType
  case object Click extends DomEventType
  case object MouseUp extends DomEventType
  case object MouseDown extends DomEventType
  case object MouseMove extends DomEventType
  case object MouseEnter extends DomEventType
  case object MouseLeave extends DomEventType

  sealed trait DomEvent { val eventType: DomEventType }

  case class Vector2d(x: Double, y: Double) {
    def -(v: Vector2d): Vector2d = Vector2d(x - v.x, y - v.y)
    def +(v: Vector2d): Vector2d = Vector2d(x + v.x, y + v.y)
  }

  sealed trait MouseEvent extends DomEvent {
    val screen: Vector2d
    val client: Vector2d
    val page: Vector2d
  }
  trait ClickEvent extends MouseEvent { final val eventType = Click }
  trait MouseUpEvent extends MouseEvent { final val eventType = MouseUp }
  trait MouseDownEvent extends MouseEvent { final val eventType = MouseDown }
  trait MouseMoveEvent extends MouseEvent { final val eventType = MouseMove }
  trait MouseEnterEvent extends MouseEvent { final val eventType = MouseEnter }
  trait MouseLeaveEvent extends MouseEvent { final val eventType = MouseLeave }

  type DomStream = Observable[DomEvent Ior EffectAction]
  type DomainStream[M] = Observable[(M, DomainEvent)]

  case class DomBinding[M](element: Element,
                           nested: Vector[DomBinding[M]] = Vector.empty,
                           domainStream: DomainStream[M] = Observable.empty,
                           domStream: DomStream = Observable.empty) {
    def mapDomStream(f: DomStream => DomStream): DomBinding[M] = copy(domStream = f(domStream))

    def flatMapElement(f: Element => ActionF[Element]): ActionF[DomBinding[M]] =
      f(element).map(e => copy(element = e))
  }

  type ActionF[A] = Free[Action, A]

  implicit class ActionOps[A](action: ActionF[A]) {
    def followedBy[B](action2: ActionF[B]): ActionF[B] = action.flatMap(_ => action2)
    def interpret[M](model: M)(implicit interpreter: ActionInterpreter): A =
      interpreter.interpret(model, action)
  }

  object Document {
    def getElementById(id: DomId): ActionF[Element] =
      value(org.scalajs.dom.document.getElementById(id.value))
  }

  object NoAction extends Action[Nothing]
  def noAction[R]: ActionF[R] = NoAction

  case class Value[A](a: A) extends Action[A]
  def value[A](a: A): ActionF[A] = Value(a)

  case class Log(text: String) extends Action[Unit]
  def log(message: String): EffectAction =
    Log(message)

  case class CreateElement(tagName: String) extends Action[Element]
  def createElement(tagName: String): ActionF[Element] = CreateElement(tagName)

  case class CreateTextNode(text: String) extends Action[Node]
  def createTextNode(text: String): ActionF[Node] = CreateTextNode(text)

  case class AppendChild[C <: Node](parent: Element, child: C) extends Action[C]
  def appendChild[C <: Node](parent: Element, child: C): ActionF[C] = AppendChild(parent, child)

  case class RemoveChild(parent: Node, child: Node) extends Action[Node]
  def removeChild(parent: Node, child: Node): ActionF[Node] = RemoveChild(parent, child)

  case class ReplaceChild(parent: Node, newChild: Node, oldChild: Node) extends Action[Node]
  def replaceChild(parent: Node, newChild: Node, oldChild: Node): ActionF[Node] =
    ReplaceChild(parent, newChild, oldChild)

  case class SetAttribute(element: Element, name: String, value: String) extends Action[Element]
  def getAttribute(name: String)(elem: Element): ActionF[Option[String]] =
    elem.getAttribute(name) match {
      case "" => value(None)
      case null => value(None)
      case v => value(Some(v))
    }
  def setAttribute(name: String, value: String)(elem: Element): ActionF[Element] = SetAttribute(elem, name, value)

  case class HandleEvents[M](element: Element, eventTypes: Iterable[DomEventType])
    extends Action[Observable[DomEvent Ior EffectAction]]
  def handleEvents[M](element: Element, eventTypes: Iterable[DomEventType]): ActionF[DomStream] =
    HandleEvents[M](element, eventTypes)

  def setId(element: Element, id: DomId) =
    setAttribute("id", id.value)(element)

  def appendText(element: Element, text: String) =
    createTextNode(text) flatMap (appendChild(element, _))

  def cssClass(element: Element): ActionF[Option[String]] = getAttribute("class")(element)

  def setClass(element: Element, class0: String) = { // TODO: test
    val class1 = class0.trim
      if (class1.isEmpty) value(element) else setAttribute("class", class1)(element)
  }

  def setClasses(element: Element, cssClasses: NonEmptyList[String]) =
    cssClasses.map(addClass(_)(element)).toList.reduce(_ followedBy _)

  def addClass(class0: String)(element: Element) = {
    val class1 = class0.trim
    if (class1.isEmpty)
      value(element)
    else
      cssClass(element)
        .map(_.fold(class1)((class0: String) => (class0.split(" ").toSet + class1).mkString(" ")))
        .flatMap(setAttribute("class", _)(element))
  }

  def addClasses(element: Element, cssClasses: NonEmptyList[String]): ActionF[Element] = {
    cssClass(element)
      .map(_.fold(cssClasses.toList.toSet)((class0: String) => cssClasses.toList ++: class0.split(" ").toSet))
      .flatMap((classes: Set[String]) => setAttribute("class", classes.toList.sorted.mkString(" "))(element))
  }

  def removeClass(class0: String)(element: Element): ActionF[Element] = {
    val class1 = class0.trim
    if (class1.isEmpty)
      value(element)
    else
      cssClass(element).flatMap(_.fold(value(element)) { class0 =>
        setAttribute("class", (class0.split(" ").toSet - class1).mkString(" "))(element)
      })
  }

  def hide(element: Element) =
    addClass("d-hidden")(element) // TODO: test

  def show(element: Element) =
    removeClass("d-hidden")(element) // TODO: test

}
