package com.github.unisay.dancher

import cats.Semigroup
import cats.syntax.xor._
import Syntax._
import cats.data.{NonEmptyVector, Xor}
import fs2._
import org.scalajs.dom.{Element, Event}

trait DomainEvent

case class Binding(element: Element, children: Vector[Binding], events: WidgetEvents = Stream.empty) {
  def map(f: Element => Element): Binding = copy(element = f(element))
}

object Binding extends BindingInstances

trait BindingInstances {
  implicit val widgetEventMapper: WidgetEventMapper[Binding] = new WidgetEventMapper[Binding] {
    def mapEvents(b: Binding, f: Flow[WidgetEvent] => Flow[WidgetEvent]): Binding = b.copy(events = f(b.events))
  }
  implicit class BindingOps(override val instance: Binding) extends WidgetEventMapperOps[Binding]
}

trait WidgetEventMapper[T] {
  def mapEvents(t: T)(f: Flow[WidgetEvent] => Flow[WidgetEvent]): T
  def mapWidgetEvent(t: T)(f: WidgetEvent => WidgetEvent): T = mapEvents(t)(_.map(f))
  def mapDomainEvent(t: T)(f: DomainEvent => DomainEvent): T = mapWidgetEvent(t)(_.map(f))
  def mapDomEvent(t: T)(f: Event => Event): T = mapWidgetEvent(t) {
    case Xor.Left(e) => f(e).left
    case right => right
  }
  def mapWidgetEventPf(t: T)(pf: PartialFunction[WidgetEvent, WidgetEvent]): T = mapWidgetEvent(t)(pf.total)
  def mapDomainEventPf(t: T)(pf: PartialFunction[DomainEvent, DomainEvent]): T = mapDomainEvent(t)(pf.total)
  def mapDomEventPf(t: T)(pf: PartialFunction[Event, Event]): T = mapDomEvent(t)(pf.total)
}

trait WidgetEventMapperOps[S] {
  val instance: S
  val mapper: WidgetEventMapper[S]
  def mapEvents(f: Flow[WidgetEvent] => Flow[WidgetEvent]): S = mapper.mapEvents(instance)(f)
  def mapWidgetEvent(f: WidgetEvent => WidgetEvent): S = mapper.mapWidgetEvent(instance)(f)
  def mapWidgetEventPf(pf: PartialFunction[WidgetEvent, WidgetEvent]): S = mapper.mapWidgetEventPf(instance)(pf)
  def mapDomainEvent(f: DomainEvent => DomainEvent): S = mapper.mapDomainEvent(instance)(f)
  def mapDomainEventPf(pf: PartialFunction[DomainEvent, DomainEvent]): S = mapper.mapDomainEventPf(instance)(pf)
  def mapDomEvent(f: Event => Event): S = mapper.mapDomEvent(instance)(f)
  def mapDomEventPf(pf: PartialFunction[Event, Event]): S = mapper.mapDomEventPf(instance)(pf)
}

case class Bindings(elements: NonEmptyVector[Element], events: Stream[Task, WidgetEvent] = Stream.empty)

object Bindings extends BindingsInstances {

  def create(binding: Binding): Bindings =
    Bindings(elements = NonEmptyVector.of(binding.element), events = binding.events)

  def create2(left: Binding, right: Binding)(implicit ec: EventsComposer): Bindings =
    bindingsSemigroup.combine(create(left), create(right))

  def append(left: Bindings, right: Binding)(implicit ec: EventsComposer) =
    bindingsSemigroup.combine(left, create(right))

  def mapElements(bindings: Bindings)(f: NonEmptyVector[Element] => NonEmptyVector[Element]): Bindings =
    bindings.copy(elements = f(bindings.elements))

  implicit class BindingsOps(override val instance: Bindings)
    extends WidgetEventMapperOps[Bindings]

}

trait BindingsInstances {
  implicit val widgetEventMapper: WidgetEventMapper[Bindings] = new WidgetEventMapper[Bindings] {
    def mapEvents(b: Bindings, f: Flow[WidgetEvent] => Flow[WidgetEvent]): Bindings = b.copy(events = f(b.events))
  }
  implicit def bindingsSemigroup(implicit ec: EventsComposer): Semigroup[Bindings] =
    new Semigroup[Bindings] {
      def combine(x: Bindings, y: Bindings) = Bindings(
        elements = x.elements concatNev y.elements,
        events = ec.compose(x.events, y.events)
      )
    }
}

case class Point(x: Double, y: Double)
