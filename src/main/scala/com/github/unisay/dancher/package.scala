package com.github.unisay

import com.github.unisay.dancher.dom.DomEvent

package object dancher {
  type DomainEventHandler = PartialFunction[(DomainEvent, Model), Model]
  type DomEventHandler = DomEvent ⇒ DomainEvent
}
