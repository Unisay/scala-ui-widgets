package com.github.unisay.dancher.widget

import cats.data.Ior._
import cats.data.{Ior, Reader}
import com.github.unisay.dancher.DomainEvent
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.interpreter.ActionInterpreter
import monix.reactive.Observable
import monocle.Lens
import org.scalajs.dom.Element
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
  def append[M](parentAction: RenderAction[M], childAction: RenderAction[M]): RenderAction[M] =
    for {
      parentBinding <- parentAction
      childBinding  <- childAction
      parentElement <- appendChild(parentBinding.element, childBinding.element)
    } yield DomBinding(
       element = parentElement,
       nested = parentBinding.nested :+ childBinding,
       domStream = Observable.merge(parentBinding.domStream, childBinding.domStream),
       domainStream = Observable.merge(parentBinding.domainStream, childBinding.domainStream)
    )
}

final class WidgetListOps[M](val widgets: List[Widget[M]]) extends AnyVal {
  def >(widget: Widget[M]): List[Widget[M]] = widgets :+ widget
}

final class WidgetOps[M](val widget: Widget[M]) {
  import RenderAction._

  def >(other: Widget[M]): List[Widget[M]] = widget :: other :: Nil

  /** Append child to parent returning parent */
  def +>(child: Widget[M]): Widget[M] = Widget(model => append(widget(model), child(model)))

  def flatMapBinding(f: DomBinding[M] => ActionF[DomBinding[M]]): Widget[M] =
    widget.map(_.flatMap(f))

  def flatMapElement(f: Element => ActionF[Element]): Widget[M] =
    flatMapBinding { (binding: DomBinding[M]) =>
      f(binding.element) map (e => binding.copy(element = e))
    }

  def render(model: M)(implicit interpreter: ActionInterpreter): DomBinding[M] =
    interpreter.interpret(model, widget(model))
}

object Widget
  extends BasicWidgets
  with LayoutWidgets
  with TabsWidget {
  implicit def widgetOps[M](widget: Widget[M]): WidgetOps[M] = new WidgetOps(widget)
  implicit def widgetListOps[M](widgets: List[Widget[M]]): WidgetListOps[M] = new WidgetListOps(widgets)
  def apply[M](f: M => RenderAction[M]): Widget[M] = Reader(f)
  def pure[M](renderAction: RenderAction[M]): Widget[M] = Widget(_ => renderAction)
  def lift[M](binding: DomBinding[M]): Widget[M] = pure(value(binding))
  def const[M, C](constant: C): Lens[M, C] = Lens[M, C](_ => constant)(_ => identity) // TODO: use magnet
}
