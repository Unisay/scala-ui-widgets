package com.github.unisay.dancher

import cats.data.Xor
import cats.syntax.xor._
import com.github.unisay.dancher.Syntax._
import com.github.unisay.dancher.Widget._
import fs2._
import org.scalajs.dom.{Element, Event}

trait DomainEvent

case class Binding(element: Element, events: WidgetEvents, nested: Vector[Binding]) extends WidgetEventMapper[Binding] {
  def deepElement: Task[Element] = nested.map(_.deepElement).foldLeft(Task.now(element)) { (pt, ct) =>
    for {
      p <- pt
      c <- ct
    } yield {
      p.appendChild(c)
      p
    }
  }

  def deepEvents: WidgetEvents = nested.map(_.deepEvents).foldLeft(events)(_ merge _)
  def mapElement(f: Element => Element): Binding = copy(element = f(element))
  def mapEvents(f: WidgetEvents => WidgetEvents): Binding = copy(events = events.through(f))
  def append(child: Binding): Binding = copy(nested = nested :+ child)
}

object Binding {
  def apply(element: Element): Binding = apply(element, Stream.empty)
  def apply(element: Element, events: WidgetEvents): Binding = Binding(element, events = events, nested = Vector.empty)
}

trait WidgetEventMapper[T] {
  def mapEvents(f: WidgetEvents => WidgetEvents): T
  def mapWidgetEvent(f: WidgetEvent => WidgetEvent): T = mapEvents(_.map(f))
  def mapDomainEvent(f: DomainEvent => DomainEvent): T = mapWidgetEvent(_.map(f))
  def mapDomEvent(f: Event => Event): T = mapWidgetEvent {
    case Xor.Left(e) => f(e).left
    case right => right
  }
  def mapWidgetEventPf(pf: PartialFunction[WidgetEvent, WidgetEvent]): T = mapWidgetEvent(pf.total)
  def mapDomainEventPf(pf: PartialFunction[DomainEvent, DomainEvent]): T = mapDomainEvent(pf.total)
  def mapDomEventPf(pf: PartialFunction[Event, Event]): T = mapDomEvent(pf.total)
}

case class Point(x: Double, y: Double)
