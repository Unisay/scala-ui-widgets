package com.github.unisay.dancher

import com.github.unisay.dancher.dom.DomEvent

trait DomainEvent
case class ClickEvent(event: DomEvent) extends DomainEvent
