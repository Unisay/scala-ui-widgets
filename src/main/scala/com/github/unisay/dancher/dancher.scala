package com.github.unisay.dancher

import cats.Semigroup
import cats.data.NonEmptyVector
import com.github.unisay.dancher.Widget.EventsComposer
import fs2.{Stream, Task}
import org.scalajs.dom.Element

trait DomainEvent

case class Binding(element: Element, events: Stream[Task, DomainEvent] = Stream.empty)

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

  def mapEvents(bindings: Bindings)(f: DomainEvents => DomainEvents): Bindings =
    bindings.copy(events = f(bindings.events))

  implicit class BindingsOps(val bindings: Bindings) extends AnyVal {
    def mapElements(f: NonEmptyVector[Element] => NonEmptyVector[Element]) = Bindings.mapElements(bindings)(f)
    def mapEvents(f: DomainEvents => DomainEvents) = Bindings.mapEvents(bindings)(f)
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
