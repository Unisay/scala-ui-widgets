package com.github.unisay.dancher

import cats.syntax.cartesian._
import fs2.Strategy
import fs2.interop.cats._
import org.scalajs.dom.Element

import scala.language.implicitConversions

object Widget {

  implicit val strategy = Strategy.default
  implicit def widgetAsFragment(widget: Widget): Fragment = widget.map(Bindings.create)

  trait EventsComposer {
    def compose(left: DomainEvents, right: DomainEvents): DomainEvents
  }

  object EventsComposer {

    def apply(f: (DomainEvents, DomainEvents) => DomainEvents): EventsComposer = new EventsComposer {
      def compose(left: DomainEvents, right: DomainEvents) = f(left, right)
    }

    val left = EventsComposer((left, _) => left)
    val right = EventsComposer((_, right) => right)
    val both = EventsComposer(_ merge _)
  }

  implicit class WidgetOps(val left: Widget) extends AnyVal {
    def mapElement(f: Element => Element): Widget = left.map(binding => binding.copy(element = f(binding.element)))
    def append(fragment: Fragment)(implicit ec: EventsComposer = EventsComposer.both): Widget =
      for {
        binding <- left
        bindings <- fragment
        element = bindings.elements.foldLeft(binding.element) { (parent, child) =>
            parent.appendChild(child)
            parent
          }
      } yield Binding(element, events = ec.compose(binding.events, bindings.events))

    def <*>(right: Widget)(implicit ec: EventsComposer = EventsComposer.both) = (left |@| right) map Bindings.create2
    def <*(right: Widget): Fragment = <*>(right)(EventsComposer.left)
    def *>(right: Widget): Fragment = <*>(right)(EventsComposer.right)
  }

  implicit class FragmentOps(val fragment: Fragment) extends AnyVal {
    def <*>(right: Widget)(implicit ec: EventsComposer = EventsComposer.both) = (fragment |@| right) map Bindings.append
    def <*(right: Widget): Fragment = <*>(right)(EventsComposer.left)
    def *>(right: Widget): Fragment = <*>(right)(EventsComposer.right)
  }

}
