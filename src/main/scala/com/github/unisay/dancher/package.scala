package com.github.unisay

import com.github.unisay.dancher.dom.DomEvent

package object dancher {
  type ModelEvent = (DomainEvent, Model)
  type DomEventHandler = DomEvent ⇒ DomainEvent
  type DomainEventHandler = PartialFunction[ModelEvent, Frame]
}
