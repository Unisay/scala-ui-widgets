package com.github.unisay.dancher

import cats.Semigroup
import cats.data.NonEmptyVector
import fs2._
import org.scalajs.dom.Element

trait DomainEvent

case class Binding(element: Element, events: DomainEvents = Stream.empty) {
  def map(f: Element => Element): Binding = copy(element = f(element))
  def pipeEvents(pipe: Pipe[Task, DomainEvent, DomainEvent]): Binding = copy(events = events.through(pipe))
}

case class Bindings(elements: NonEmptyVector[Element], events: Stream[Task, DomainEvent] = Stream.empty)

object Bindings extends BindingsInstances {

  def create(binding: Binding): Bindings =
    Bindings(elements = NonEmptyVector.of(binding.element), events = binding.events)

  def create2(left: Binding, right: Binding)(implicit ec: EventsComposer): Bindings =
    bindingsSemigroup.combine(create(left), create(right))

  def append(left: Bindings, right: Binding)(implicit ec: EventsComposer) =
    bindingsSemigroup.combine(left, create(right))

  def mapElements(bindings: Bindings)(f: NonEmptyVector[Element] => NonEmptyVector[Element]): Bindings =
    bindings.copy(elements = f(bindings.elements))

  def pipeEvents(bindings: Bindings)(pipe: Pipe[Task, DomainEvent, DomainEvent]): Bindings =
    bindings.copy(events = bindings.events.through(pipe))

  implicit class BindingsOps(val bindings: Bindings) extends AnyVal {
    def pipeEvents(pipe: Pipe[Task, DomainEvent, DomainEvent]) = Bindings.pipeEvents(bindings)(pipe)
  }

}

trait BindingsInstances {
  implicit def bindingsSemigroup(implicit ec: EventsComposer): Semigroup[Bindings] =
    new Semigroup[Bindings] {
      def combine(x: Bindings, y: Bindings) = Bindings(
        elements = x.elements concatNev y.elements,
        events = ec.compose(x.events, y.events)
      )
    }
}

case class Point(x: Double, y: Double)
