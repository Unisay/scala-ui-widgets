package com.github.unisay.dancher

import com.github.unisay.dancher.Widget._
import fs2._
import org.scalajs.dom.{Element, Event}

trait DomainEvent

case class Binding(element: Element, domEvents: Flow[Event], domainEvents: Flow[DomainEvent], nested: Vector[Binding]) {
  def mapElement(f: Element => Element) = copy(element = f(element))
  def mapDomEvent(f: Event => Event) = pipeDomEvents(_.map(f))
  def pipeDomEvents(pipe: Flow[Event] => Flow[Event]) = copy(domEvents = domEvents.through(pipe))
  def mapDomainEvent(f: DomainEvent => DomainEvent) = pipeDomainEvents(_.map(f))
  def pipeDomainEvents(pipe: Flow[DomainEvent] => Flow[DomainEvent]) = copy(domainEvents = domainEvents.through(pipe))
  def handleDomEvents(pipe: Flow[Event] => Flow[DomainEvent]) =
    copy(domEvents = Stream.empty, domainEvents = domainEvents.merge(domEvents.through(pipe)))
  def append(child: Binding): Task[Binding] =
    Task.delay {
      element.appendChild(child.element)
      copy(
        domEvents = domEvents.merge(child.domEvents),
        domainEvents = domainEvents.merge(child.domainEvents),
        nested = nested :+ child)
    }
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
