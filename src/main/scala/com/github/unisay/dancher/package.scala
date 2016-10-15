package com.github.unisay

import eu.timepit.refined.api.Refined
import fs2.{Stream, Task}
import fs2.interop.cats._
import cats.syntax.functor._

package object dancher {

  type Effect = Task[Unit]
  type Widget = Task[Binding]
  type Fragment = Task[List[Binding]]
  type Flow[T] = Stream[Task, T]
  type Is[T, P] = Refined[T, P]

  val NoEffect: Effect = Task.now(())
  def Effect[A](a: => A): Effect = Task.delay(a).void
}


