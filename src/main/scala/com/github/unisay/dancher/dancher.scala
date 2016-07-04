package com.github.unisay.dancher

import com.github.unisay.dancher.dom.{ActionF, DomEvent, DomNode}
import monix.reactive.Observable

case class Frame(model: Model, action: ActionF[DomBinding])
case class DomBinding(node: DomNode, events: Option[Observable[ModelEvent]] = None)

trait DomainEvent
case class ClickEvent(event: DomEvent) extends DomainEvent
