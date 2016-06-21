package com.github.unisay

import com.github.unisay.dancher.dom.DomEvent
import monix.reactive.Observable

package object dancher {
  type ModelEvent = (DomainEvent, ModelBuilder)
  type ModelEvents = Observable[ModelEvent]
  type DomainEventHandler = PartialFunction[ModelEvent, ModelBuilder]
  type DomEventHandler = DomEvent ⇒ DomainEvent
}
