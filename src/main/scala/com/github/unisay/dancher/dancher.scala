package com.github.unisay.dancher

import cats.data.{NonEmptyVector, Xor}
import cats.syntax.xor._
import com.github.unisay.dancher.Syntax._
import fs2._
import org.scalajs.dom.{Element, Event}

trait DomainEvent

case class Binding(element: Element, events: WidgetEvents, nested: Vector[Binding]) {
  def deepElements: NonEmptyVector[Element] = NonEmptyVector.of(element, nested.flatMap(_.deepElements.toVector): _*)
  def deepEvents: WidgetEvents = EventsComposer.both.composeAll(events +: nested.map(_.events): _*)
  def mapElement(f: Element => Element): Binding = copy(element = f(element))
  def append(child: Binding): Binding = copy(nested = nested :+ child)
}

object Binding extends BindingInstances {
  def apply(element: Element): Binding = apply(element, Stream.empty)
  def apply(element: Element, events: WidgetEvents): Binding = Binding(element, events = events, nested = Vector.empty)
}

trait BindingInstances {
  implicit val widgetEventMapper: WidgetEventMapper[Binding] = new WidgetEventMapper[Binding] {
    def mapEvents(b: Binding)(pipe: WidgetEvents => WidgetEvents): Binding = b.copy(events = b.events.through(pipe))
  }
  implicit class BindingOps(override val instance: Binding) extends WidgetEventMapperOps[Binding] {
    val mapper: WidgetEventMapper[Binding] = widgetEventMapper
  }
}

// TODO - consider inlining this TC?
trait WidgetEventMapper[T] {
  def mapEvents(t: T)(f: WidgetEvents => WidgetEvents): T
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

case class Point(x: Double, y: Double)
