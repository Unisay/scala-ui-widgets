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
  def append[M](parent: RenderAction, child: RenderAction): RenderAction =
    for {
      pb <- parent
      cb  <- child
      parentElement <- appendChild(pb.element, cb.element)(cb.elementEvidence, pb.elementEvidence)
    } yield DomBinding(
       parentElement,
       pb.nested :+ cb,
       Observable.merge(pb.events, cb.events)
    )(cb.elementEvidence)
}

final class WidgetListOps[M](val widgets: List[Widget[M]]) extends AnyVal {
  def >(widget: Widget[M]): List[Widget[M]] = widgets :+ widget
}

final class WidgetOps[M](val widget: Widget[M]) extends AnyVal {
  import RenderAction._

  def >(other: Widget[M]): List[Widget[M]] = widget :: other :: Nil

  /** Append child to parent returning parent */
  def +>(child: Widget[M]): Widget[M] = Widget(model => append(widget(model), child(model)))

  def flatMapBinding(f: DomBinding => ActionF[DomBinding]): Widget[M] =
    widget.map(_.flatMap(f))

  def flatMapElement(f: (DomBinding#E, DomElem[DomBinding#E]) => ActionF[DomBinding#E]): Widget[M] =
    flatMapBinding { (binding: DomBinding) =>
      f(binding.element, binding.elementEvidence).map { (e: DomBinding#E) =>
        DomBinding(e, binding.nested, binding.events)(binding.elementEvidence)
      }
    }

  def render(model: M)(implicit interpreter: ActionInterpreter): DomBinding =
    interpreter.interpret(model, widget(model))
}

object Widget
  extends BasicWidgets
  with LayoutWidgets
  with TabsWidget {
  implicit def widgetOps[M](widget: Widget[M]): WidgetOps[M] = new WidgetOps[M](widget)
  implicit def widgetListOps[M](widgets: List[Widget[M]]): WidgetListOps[M] = new WidgetListOps(widgets)
  def apply[M](f: M => RenderAction): Widget[M] = Reader(f)
  def pure[M](renderAction: RenderAction): Widget[M] = Widget(_ => renderAction)
  def lift[M](binding: DomBinding): Widget[M] = pure(value(binding))
  def const[M, C](constant: C): Lens[M, C] = Lens[M, C](_ => constant)(_ => identity) // TODO: use magnet
}
