package com.github.unisay.dancher

import fs2.Strategy

trait EventsComposer {
  def compose(left: WidgetEvents, right: WidgetEvents): WidgetEvents
}

object EventsComposer {

  implicit val strategy = Strategy.default

  def apply(f: (WidgetEvents, WidgetEvents) => WidgetEvents): EventsComposer =
    new EventsComposer {
      def compose(left: WidgetEvents, right: WidgetEvents) = f(left, right)
    }

  val left = EventsComposer((left, _) => left)
  val right = EventsComposer((_, right) => right)
  val both = EventsComposer(_ merge _)

}
