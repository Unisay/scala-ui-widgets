package com.github.unisay.dancher

import com.github.unisay.dancher.DomSyntax._
import fs2.{Strategy, Task}
import Syntax._
import cats.implicits._
import fs2.interop.cats._
import org.scalajs.dom.Element

object Widget extends WidgetInstances {

  implicit val strategy = Strategy.default

  implicit class WidgetsOps(val widgets: List[Widget]) extends AnyVal {
    def mapAll(pf: PartialFunction[List[Binding], List[Binding]]): Task[List[Binding]] = widgets.sequence.map(pf.total)
  }

  implicit class WidgetOps(override val instance: Widget) extends WidgetEventMapperOps[Widget] {

    def element = instance.map(_.element)

    def mapElement(f: Element => Element): Widget = instance.map(_.mapElement(f))

    def emitDomEvents(eventTypes: Dom.Event.Type*): Widget =
      instance.map { binding =>
        val domEvents = binding.element.stream(eventTypes: _*).map(_.left)
        binding.copy(events = EventsComposer.both.compose(binding.events, domEvents))
      }

    def append(child: Binding): Widget = instance.map(_ append child)
    def append(widget: Widget): Widget = instance.flatMap(append)
    def append(widgets: Task[List[Binding]]): Widget =
      for {
        bs <- widgets
        binding <- instance
      } yield bs.foldLeft(binding)(_ append _)

    def append(widgets: List[Widget]): Widget =
      widgets.foldLeft(instance)(_ append _)

    def ::(right: Widget) = instance :: right :: Nil

    def setClass(classes: String*): Widget = instance.mapElement(_.setClass(classes: _*))
  }

}

trait WidgetInstances {
  implicit val widgetEventMapper: WidgetEventMapper[Widget] = new WidgetEventMapper[Widget] {
    def mapEvents(w: Widget, f: (Flow[WidgetEvent]) => Flow[WidgetEvent]): Widget =
      w.map(implicitly[WidgetEventMapper[Binding]].mapEvents(_)(f))
  }
}
