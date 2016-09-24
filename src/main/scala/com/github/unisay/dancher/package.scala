package com.github.unisay

import cats.data.Xor
import eu.timepit.refined.api.Refined
import fs2.{Stream, Task}
import org.scalajs.dom.Event

package object dancher {

  type Effect = Task[Unit]
  type Widget = Task[Binding]
  type Fragment = Task[List[Binding]]
  type Flow[T] = Stream[Task, T]
  type WidgetEvent = Event Xor DomainEvent
  type WidgetEvents = Flow[WidgetEvent]
  type Is[T, P] = Refined[T, P]

  val EmptyEffect: Effect = Task.now(())
  def Effect[A](a: => A): Task[A] = Task.delay(a)
}


