package com.github.unisay

import com.github.unisay.dancher.dom.DomEvent
import monix.reactive.Observable

package object dancher {
  type ModelEvent = (DomainEvent, Model)
  type ModelEvents = Observable[ModelEvent]
  type DomEventHandler = DomEvent ⇒ DomainEvent
}
