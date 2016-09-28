package com.github.unisay.dancher

import com.github.unisay.dancher.Widget._
import fs2._
import org.scalajs.dom.Element

trait DomainEvent

case class Binding(element: Element, events: Flow[WidgetEvent], nested: Vector[Binding]) {
  def deepElement: Task[Element] = nested.map(_.deepElement).foldLeft(Task.now(element)) { (pt, ct) =>
    for {
      p <- pt
      c <- ct
    } yield {
      p.appendChild(c)
      p
    }
  }

  def deepEvents: Flow[WidgetEvent] = nested.map(_.deepEvents).foldLeft(events)(_ merge _)
  def mapElement(f: Element => Element): Binding = copy(element = f(element))
  def mapWidgetEvents(pipe: Flow[WidgetEvent] => Flow[WidgetEvent]): Binding = copy(events = events.through(pipe))
  def mapWidgetEvent(f: WidgetEvent => WidgetEvent): Binding = copy(events = events.map(f))
  def mapDomainEvent(f: DomainEvent => DomainEvent): Binding = mapWidgetEvent(_.map(f))
  def append(child: Binding): Binding = copy(nested = nested :+ child)
  def render: Widget = deepElement.map(e => copy(element = e))
}

object Binding {
  def apply(element: Element): Binding =
    apply(element, Stream.empty)
  def apply(element: Element, events: Flow[WidgetEvent]): Binding =
    Binding(element, events = events, nested = Vector.empty)
}

case class Point(x: Double, y: Double)
