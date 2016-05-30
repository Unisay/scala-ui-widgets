package com.github.unisay

package object dancher {
  type DomainEventHandler = PartialFunction[(DomainEvent, Model), Model]
}
