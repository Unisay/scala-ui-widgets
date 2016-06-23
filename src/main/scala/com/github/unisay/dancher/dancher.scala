package com.github.unisay.dancher

import com.github.unisay.dancher.dom.DomNode

case class DomBinding(node: DomNode, events: Option[ModelEvents] = None)

trait DomainEvent
