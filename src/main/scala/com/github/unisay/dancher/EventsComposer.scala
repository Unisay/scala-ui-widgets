package com.github.unisay.dancher

import fs2.Strategy

trait EventsComposer {
  def compose(left: DomainEvents, right: DomainEvents): DomainEvents
}

object EventsComposer {

  implicit val strategy = Strategy.default

  def apply(f: (DomainEvents, DomainEvents) => DomainEvents): EventsComposer =
    new EventsComposer {
      def compose(left: DomainEvents, right: DomainEvents) = f(left, right)
    }

  val left = EventsComposer((left, _) => left)
  val right = EventsComposer((_, right) => right)
  val both = EventsComposer(_ merge _)

}
