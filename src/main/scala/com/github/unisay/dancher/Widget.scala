package com.github.unisay.dancher

import cats.syntax.all._
import com.github.unisay.dancher.DomSyntax._
import fs2.interop.cats._
import fs2.{Pipe, Strategy, Task}
import org.scalajs.dom.{Element, Event}

import scala.language.implicitConversions

object Widget extends WidgetInstances {

  implicit val strategy = Strategy.default
  implicit def widgetAsFragment(widget: Widget): Fragment = widget.map(Bindings.create)

  implicit class WidgetOps(override val instance: Widget) extends WidgetEventMapperOps[Widget] {

    def element = instance.map(_.element)

    def mapElement(f: Element => Element): Widget =
      instance.map(_.map(f))

    def pipeDomEvents[O](eventTypes: Dom.Event.Type*)
                        (pipe: Pipe[Task, Event, O])
                        (implicit eventsComposer: EventsComposer): Widget =
      instance.map { (binding: Binding) =>
        binding.mapEvents { (widgetEvents: WidgetEvents) =>
          eventsComposer.compose(widgetEvents, binding.element.stream(eventTypes: _*).map(_.left))
        }
      }

    def append(fragment: Fragment)(implicit ec: EventsComposer = EventsComposer.both): Widget =
      for {
        binding <- instance
        bindings <- fragment
      } yield Binding(
        element = binding.element.appendAll(bindings.elements.toVector),
        events = ec.compose(binding.events, bindings.events)
      )

    def <*>(right: Widget)(implicit ec: EventsComposer = EventsComposer.both) =
      (instance |@| right) map Bindings.create2

    def <*(right: Widget): Fragment =
      <*>(right)(EventsComposer.left)

    def *>(right: Widget): Fragment =
      <*>(right)(EventsComposer.right)

    def setClass(classes: String*): Widget = instance.mapElement(_.setClass(classes: _*))
  }

}

trait WidgetInstances {
  implicit val widgetEventMapper: WidgetEventMapper[Widget] = new WidgetEventMapper[Widget] {
    def mapEvents(w: Widget, f: (Flow[WidgetEvent]) => Flow[WidgetEvent]): Widget =
      w.map(implicitly[WidgetEventMapper[Binding]].mapEvents(_)(f))
  }
}
