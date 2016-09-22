package com.github.unisay.dancher

import cats.syntax.all._
import cats.instances.list._
import com.github.unisay.dancher.DomSyntax._
import fs2.Strategy
import fs2.interop.cats._
import org.scalajs.dom.Element

object Widget extends WidgetInstances {

  implicit val strategy = Strategy.default

  implicit class WidgetsOps(val widgets: List[Widget]) extends AnyVal {
    def mapAll(f: List[Binding] => List[Binding]): List[Widget] = widgets.sequence.map(f)
  }

  implicit class WidgetOps(override val instance: Widget) extends WidgetEventMapperOps[Widget] {

    def element = instance.map(_.element)

    def mapElements(f: Element => Element): Widget =
      instance.map(_.mapElements(f))

    def emitDomEvents(eventTypes: Dom.Event.Type*)(implicit ec: EventsComposer): Widget =
      instance.map { binding =>
        val domEvents = binding.element.stream(eventTypes: _*).map(_.left)
        binding.copy(events = ec.compose(binding.events, domEvents))
      }

    def append(widget: Widget): Widget =
      instance.flatMap { parent =>
        widget.map { child =>
          parent.copy(nested = parent.nested :+ child)
        }
      }

    def append(widgets: List[Widget]): Widget =
      widgets.foldLeft(instance)(append)

    def ::(right: Widget) = instance :: right :: Nil

    def setClass(classes: String*): Widget = instance.mapElements(_.setClass(classes: _*))
  }

}

trait WidgetInstances {
  implicit val widgetEventMapper: WidgetEventMapper[Widget] = new WidgetEventMapper[Widget] {
    def mapEvents(w: Widget, f: (Flow[WidgetEvent]) => Flow[WidgetEvent]): Widget =
      w.map(implicitly[WidgetEventMapper[Binding]].mapEvents(_)(f))
  }
}
