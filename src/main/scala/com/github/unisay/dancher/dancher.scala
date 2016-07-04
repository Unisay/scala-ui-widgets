package com.github.unisay.dancher

import com.github.unisay.dancher.dom.{DomEvent, DomNode}

case class DomBinding(node: DomNode, events: Option[ModelEvents] = None)

trait DomainEvent
case class ClickEvent(event: DomEvent) extends DomainEvent
