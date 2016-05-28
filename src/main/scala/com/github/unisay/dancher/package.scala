package com.github.unisay

import scalaz.Free

package object dancher {
  type DomEventHandler[E <: DomEvent, C] = E ⇒ C // TODO: C[E] ?
  type DomMouseEventHandler[C] = DomEventHandler[DomMouseEvent, C]
  type DomActionF[A] = Free[DomAction, A]
  type DomainEventHandler = PartialFunction[(DomainEvent, Model), Model]
}
