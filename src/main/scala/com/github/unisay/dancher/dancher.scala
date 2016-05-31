package com.github.unisay.dancher

import com.github.unisay.dancher.dom.DomEvent

trait DomainEvent

object Unhandled extends DomainEvent

object NoEventHandler extends DomEventHandler {
  def apply(domEvent: DomEvent): DomainEvent = Unhandled
}