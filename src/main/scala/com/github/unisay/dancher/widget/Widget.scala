package com.github.unisay.dancher.widget

import cats.data.Ior._
import cats.data.{Ior, Reader}
import com.github.unisay.dancher.DomainEvent
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.interpreter.ActionInterpreter
import monix.reactive.Observable
import monocle.Lens

import scala.language.implicitConversions

object HandlerResult {
  type HandlerResult[M] = (M, EffectAction Ior DomainEvent)
  def apply[M](model: M, domainEvent: DomainEvent): HandlerResult[M] =
    (model, right(domainEvent))
  def apply[M](model: M, action: EffectAction): HandlerResult[M] =
    (model, left(action))
  def apply[M](model: M, domainEvent: DomainEvent, action: EffectAction): HandlerResult[M] =
    (model, both(action, domainEvent))
}

object RenderAction {
  def append[E: DomElem, M](parent: RenderAction[E, M], child: RenderAction[E, M]): RenderAction[E, M] =
    for {
      pb <- parent
      cb  <- child
      parentElement <- appendChild(pb.element, cb.element)
    } yield DomBinding(
       parentElement,
       pb.nested :+ cb,
       Observable.merge(pb.domainStream, cb.domainStream)
    )
}

final class WidgetListOps[E, M](val widgets: List[Widget[E, M]]) extends AnyVal {
  def >(widget: Widget[E, M]): List[Widget[E, M]] = widgets :+ widget
}

final class WidgetOps[E: DomElem, M](val widget: Widget[E, M]) {
  import RenderAction._

  def >(other: Widget[E, M]): List[Widget[E, M]] = widget :: other :: Nil

  /** Append child to parent returning parent */
  def +>(child: Widget[E, M]): Widget[E, M] = Widget(model => append(widget(model), child(model)))

  def flatMapBinding(f: DomBinding[E, M] => ActionF[DomBinding[E, M]]): Widget[E, M] =
    widget.map(_.flatMap(f))

  def flatMapElement(f: E => ActionF[E]): Widget[E, M] =
    flatMapBinding { (binding: DomBinding[E, M]) =>
      f(binding.element) map (e => binding.copy(element = e))
    }

  def render(model: M)(implicit interpreter: ActionInterpreter): DomBinding[E, M] =
    interpreter.interpret(model, widget(model))
}

object Widget
  extends BasicWidgets
  with LayoutWidgets
  with TabsWidget {
  implicit def widgetOps[E: DomElem, M](widget: Widget[E, M]): WidgetOps[E, M] = new WidgetOps(widget)
  implicit def widgetListOps[E: DomElem, M](widgets: List[Widget[E, M]]): WidgetListOps[E, M] = new WidgetListOps(widgets)
  def apply[E, M](f: M => RenderAction[E, M]): Widget[E, M] = Reader(f)
  def pure[E, M](renderAction: RenderAction[E, M]): Widget[E, M] = Widget(_ => renderAction)
  def lift[E, M](binding: DomBinding[E, M]): Widget[E, M] = pure(value(binding))
  def const[M, C](constant: C): Lens[M, C] = Lens[M, C](_ => constant)(_ => identity) // TODO: use magnet
}
