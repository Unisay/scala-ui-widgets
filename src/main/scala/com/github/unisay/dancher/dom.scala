package com.github.unisay.dancher

import java.util.UUID

import cats.data.{Ior, NonEmptyList}
import cats.free.Free
import com.github.unisay.dancher.interpreter.ActionInterpreter
import com.github.unisay.dancher.widget.EffectAction
import monix.reactive.Observable

import scala.language.implicitConversions

object dom {

  implicit def actionToFree[A](action: Action[A]): ActionF[A] = Free.liftF(action)

  sealed trait Action[+N]

  object DomId {
    def unique: DomId = DomId(UUID.randomUUID().toString)
  }

  case class DomId(value: String) extends AnyVal

  trait DomNode[+N]
  trait DomElem[+E] extends DomNode[E]

  sealed trait DomEventType
  case object Click extends DomEventType
  case object MouseUp extends DomEventType
  case object MouseDown extends DomEventType
  case object MouseMove extends DomEventType
  case object MouseEnter extends DomEventType
  case object MouseLeave extends DomEventType

  sealed trait DomEvent { val eventType: DomEventType }
  trait ClickEvent extends DomEvent { final val eventType = Click }
  trait MouseUpEvent extends DomEvent { final val eventType = MouseUp }
  trait MouseDownEvent extends DomEvent { final val eventType = MouseDown }
  trait MouseMoveEvent extends DomEvent { final val eventType = MouseMove }
  trait MouseEnterEvent extends DomEvent { final val eventType = MouseEnter }
  trait MouseLeaveEvent extends DomEvent { final val eventType = MouseLeave }

  trait DomNodeList

  trait DomBinding {
    type M // Model
    type E // DOM Element
    implicit val elementEvidence: DomElem[E]
    val element: E
    val nested: Vector[DomBinding]
    val events: Observable[(M, DomainEvent)]
    val actions: Observable[DomEvent Ior EffectAction]

    def flatMapElement(f: E => ActionF[E]): ActionF[DomBinding] =
    f(element).map(ee => DomBinding(ee, nested, events))
  }

  object DomBinding {
    def apply[M0, E0](element: E0,
                      nested: Vector[DomBinding] = Vector.empty,
                      domainStream: Observable[(M0, DomainEvent)] = Observable.empty,
                      domStream: Observable[DomEvent Ior EffectAction] = Observable.empty)
                     (implicit elementEv: DomElem[E0]): DomBinding = {
      val _element = element
      val _nested = nested
      val _events = domainStream
      val _actions = domStream
      new DomBinding {
        type M = M0
        type E = E0
        val elementEvidence = elementEv
        val element = _element
        val events = _events
        val nested = _nested
        val actions = _actions
      }
    }
  }

  type ActionF[A] = Free[Action, A]

  implicit class ActionOps[A](action: ActionF[A]) {
    def followedBy[B](action2: ActionF[B]): ActionF[B] = action.flatMap(_ => action2)
    def interpret[M](model: M)(implicit interpreter: ActionInterpreter): A =
      interpreter.interpret(model, action)
  }

  object NoAction extends Action[Nothing]
  def noAction[R]: ActionF[R] = NoAction

  case class Value[A](a: A) extends Action[A]
  def value[A](a: A): ActionF[A] = Value(a)

  case class Log(text: String) extends Action[Unit]
  def log(message: String): EffectAction =
    Log(message)

  class GetDocumentBody[E] extends Action[E]
  def getDocumentBody[E: DomElem]: ActionF[E] = new GetDocumentBody[E]

  case class GetElementById[E: DomElem](id: DomId) extends Action[E]
  def getElementById[E: DomElem](id: DomId): ActionF[E] = GetElementById[E](id)

  case class GetElementsByName(Name: String) extends Action[DomNodeList]
  def getElementsByName(Name: String): ActionF[DomNodeList] = GetElementsByName(Name)

  case class GetElementsByTagName(tagName: String) extends Action[DomNodeList]
  def getElementsByTagName(tagName: String): ActionF[DomNodeList] = GetElementsByTagName(tagName)

  case class GetElementsByClassName(className: String) extends Action[DomNodeList]
  def getElementsByClassName(className: String): ActionF[DomNodeList] = GetElementsByClassName(className)

  case class CreateElement[E: DomElem](tagName: String) extends Action[E]
  def createElement[E: DomElem](tagName: String): ActionF[E] = CreateElement[E](tagName)

  case class CreateTextNode[N: DomNode](text: String) extends Action[N]
  def createTextNode[N: DomNode](text: String): ActionF[N] = CreateTextNode[N](text)

  case class GetParent[N: DomNode](node: N) extends Action[N]
  def getParentNode[N: DomNode](node: N): ActionF[N] = GetParent(node)

  case class GetFirstChild[N: DomNode](node: N) extends Action[N]
  def getFirstChild[N: DomNode](node: N): ActionF[N] = GetFirstChild(node)

  case class AppendChild[N: DomNode, E: DomElem](parent: E, child: N) extends Action[N]
  def appendChild[N: DomNode, E: DomElem](parent: E, child: N): ActionF[N] = AppendChild(parent, child)

  case class RemoveChild[N: DomNode](parent: N, child: N) extends Action[N]
  def removeChild[N: DomNode](parent: N, child: N): ActionF[N] = RemoveChild(parent, child)

  case class ReplaceChild[N: DomNode](parent: N, newChild: N, oldChild: N) extends Action[N]
  def replaceChild[N: DomNode](parent: N, newChild: N, oldChild: N): ActionF[N] =
    ReplaceChild(parent, newChild, oldChild)

  case class GetAttribute[E: DomElem](element: E, name: String) extends Action[Option[String]]
  def getAttribute[E: DomElem](name: String)(element: E): ActionF[Option[String]] = GetAttribute[E](element, name)

  case class SetAttribute[E: DomElem](element: E, name: String, value: String) extends Action[E]
  def setAttribute[E: DomElem](name: String, value: String)(elem: E): ActionF[E] = SetAttribute(elem, name, value)

  case class HandleEvents[M, E: DomElem](element: E, eventTypes: Iterable[DomEventType])
    extends Action[Observable[DomEvent Ior EffectAction]]
  def handleEvents[M, E: DomElem](element: E, eventTypes: Iterable[DomEventType]): ActionF[Observable[DomEvent Ior EffectAction]] =
    HandleEvents[M, E](element, eventTypes)

  def getId[E: DomElem](element: E) =
    getAttribute("id")(element) /* TODO: get element id directly */

  def setId[E: DomElem](element: E, id: DomId) =
    setAttribute("id", id.value)(element)

  def appendText[N: DomNode, E: DomElem](element: E, text: String) =
    createTextNode[N](text) flatMap (appendChild(element, _))

  def cssClass[E: DomElem](element: E): ActionF[Option[String]] = getAttribute("class")(element)

  def setClass[E: DomElem](element: E, class0: String) = { // TODO: test
    val class1 = class0.trim
      if (class1.isEmpty) value(element) else setAttribute("class", class1)(element)
  }

  def setClasses[E: DomElem](element: E, cssClasses: NonEmptyList[String]) =
    cssClasses.map(addClass(_)(element)).toList.reduce(_ followedBy _)

  def addClass[E: DomElem](class0: String)(element: E) = {
    val class1 = class0.trim
    if (class1.isEmpty)
      value(element)
    else
      cssClass(element)
        .map(_.fold(class1)((class0: String) => (class0.split(" ").toSet + class1).mkString(" ")))
        .flatMap(setAttribute("class", _)(element))
  }

  def addClasses[E: DomElem](element: E, cssClasses: NonEmptyList[String]): ActionF[E] = {
    cssClass(element)
      .map(_.fold(cssClasses.toList.toSet)((class0: String) => cssClasses.toList ++: class0.split(" ").toSet))
      .flatMap((classes: Set[String]) => setAttribute("class", classes.toList.sorted.mkString(" "))(element))
  }

  def removeClass[E: DomElem](class0: String)(element: E): ActionF[E] = {
    val class1 = class0.trim
    if (class1.isEmpty)
      value(element)
    else
      cssClass(element).flatMap(_.fold(value(element)) { class0 =>
        setAttribute("class", (class0.split(" ").toSet - class1).mkString(" "))(element)
      })
  }

  def hide[E: DomElem](element: E) =
    addClass("d-hidden")(element) // TODO: test

  def show[E: DomElem](element: E) =
    removeClass("d-hidden")(element) // TODO: test

}
