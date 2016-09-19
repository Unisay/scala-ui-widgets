package com.github.unisay

import eu.timepit.refined.api.Refined
import fs2.{Stream, Task}

package object dancher {

  type Widget = Task[Binding]
  type Fragment = Task[Bindings]
  type DomainEvents = Stream[Task, DomainEvent]
  type Is[T, P] = Refined[T, P]

}
