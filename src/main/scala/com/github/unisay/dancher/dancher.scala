package com.github.unisay.dancher

import com.github.unisay.dancher.dom.DomEvent

trait DomainEvent
object Render extends DomainEvent
case class ClickEvent(event: DomEvent) extends DomainEvent
