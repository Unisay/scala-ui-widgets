package com.github.unisay

import com.github.unisay.dancher.dom.DomEvent

package object dancher {
  type ModelEvent = (DomainEvent, Model)
  type DomainEventHandler = PartialFunction[ModelEvent, Model]
  type DomEventHandler = DomEvent ⇒ DomainEvent
}
