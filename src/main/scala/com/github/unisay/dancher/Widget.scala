package com.github.unisay.dancher

import cats.syntax.cartesian._
import com.github.unisay.dancher.DomSyntax._
import fs2.interop.cats._
import fs2.{Pipe, Strategy, Stream, Task}
import org.scalajs.dom.{Element, Event}

import scala.language.implicitConversions

object Widget {

  implicit val strategy = Strategy.default
  implicit def widgetAsFragment(widget: Widget): Fragment = widget.map(Bindings.create)

  implicit class WidgetOps(val widget: Widget) extends AnyVal {

    def mapElement(f: Element => Element): Widget =
      widget.map(_.map(f))

    def pipeEvents(pipe: Pipe[Task, DomainEvent, DomainEvent]): Widget =
      widget.map(_.pipeEvents(pipe))

    def pipeDomEvents(eventTypes: Dom.Event.Type*)(pipe: Pipe[Task, Event, DomainEvent]): Widget =
      widget.map { binding => binding.copy(events = binding.element.stream(eventTypes: _*).through(pipe)) }

    def mapEvent(pf: PartialFunction[DomainEvent, DomainEvent]): Widget =
      pipeEvents(_.map(pf.applyOrElse(_, identity[DomainEvent])))

    def append(fragment: Fragment)(implicit ec: EventsComposer = EventsComposer.both): Widget =
      for {
        binding <- widget
        bindings <- fragment
      } yield Binding(
        element = binding.element.appendAll(bindings.elements.toVector),
        events = ec.compose(binding.events, bindings.events)
      )

    def <*>(right: Widget)(implicit ec: EventsComposer = EventsComposer.both) =
      (widget |@| right) map Bindings.create2

    def <*(right: Widget): Fragment =
      <*>(right)(EventsComposer.left)

    def *>(right: Widget): Fragment =
      <*>(right)(EventsComposer.right)

    def setClass(classes: String*): Widget = widget.mapElement(_.setClass(classes: _*))
  }

}
