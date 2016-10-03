package com.github.unisay

import eu.timepit.refined.api.Refined
import fs2.{Stream, Task}

package object dancher {

  type Effect = Task[Unit]
  type Widget = Task[Binding]
  type Fragment = Task[List[Binding]]
  type Flow[T] = Stream[Task, T]
  type Is[T, P] = Refined[T, P]

  val EmptyEffect: Effect = Task.now(())
  def Effect[A](a: => A): Task[A] = Task.delay(a)
}


