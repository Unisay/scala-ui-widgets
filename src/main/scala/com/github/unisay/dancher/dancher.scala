package com.github.unisay.dancher

import cats.data.Ior
import com.github.unisay.dancher.Widget._
import fs2._
import org.scalajs.dom.{Element, Event}

trait DomainEvent

case class Binding(element: Element,
                   domEvents: Flow[Event],
                   domainEvents: Flow[DomainEvent],
                   nested: Vector[Binding]) {
  def render: Widget = deepElement.map(e => copy(element = e))
  def deepElement: Task[Element] = nested.map(_.deepElement).foldLeft(Task.now(element)) { (pt, ct) =>
    for {
      p <- pt
      c <- ct
    } yield {
      p.appendChild(c)
      p
    }
  }
  def mapElement(f: Element => Element): Binding = copy(element = f(element))

  def deepDomEvents: Flow[Event] = nested.map(_.deepDomEvents).foldLeft(domEvents)(_ merge _)
  def deepDomainEvents: Flow[DomainEvent] = nested.map(_.deepDomainEvents).foldLeft(domainEvents)(_ merge _)
  def pipeDomEvents(pipe: Flow[Event] => Flow[Event]) =
    new Binding(element, domEvents, domainEvents, nested) {
      override def deepDomEvents: Flow[Event] = super.deepDomEvents.through(pipe)
    }

  def pipeDomainEvents(pipe: Flow[DomainEvent] => Flow[DomainEvent]) =
    new Binding(element, domEvents, domainEvents, nested) {
      override def deepDomainEvents: Flow[DomainEvent] = super.deepDomainEvents.through(pipe)
    }

  def mapDomEvent(f: Event => Event) = pipeDomEvents(_.map(f))
  def mapDomEventToDomain(f: Event => Event Ior DomainEvent) = {
    val events = domEvents.map(f)
    val domEvents1 = events.flatMap(_.left.fold(Stream.empty[Task, Event])(Stream(_)))
    val domainEvents1 = events.flatMap(_.right.fold(Stream.empty[Task, DomainEvent])(Stream(_)))
    copy(domEvents = domEvents1, domainEvents = domainEvents1)
  }
  def mapDomainEvent(f: DomainEvent => DomainEvent) = pipeDomainEvents(_.map(f))
  def append(child: Binding): Binding = copy(nested = nested :+ child)
}

object Binding {
  def apply(element: Element): Binding =
    apply(element, Stream.empty)
  def apply(element: Element, domEvents: Flow[Event]): Binding =
    Binding(element, domEvents, domainEvents = Stream.empty)
  def apply(element: Element, domEvents: Flow[Event], domainEvents: Flow[DomainEvent]): Binding =
    Binding(element, domEvents, domainEvents, nested = Vector.empty)
}

case class Point(x: Double, y: Double)
