package com.github.unisay.dancher

import com.github.unisay.dancher.dom.DomElement

case class DomBinding(element: DomElement, events: Option[ModelEvents] = None)

trait DomainEvent