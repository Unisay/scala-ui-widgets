package com.github.unisay.dancher

import cats.implicits._
import com.github.unisay.dancher.DomSyntax._
import com.github.unisay.dancher.Syntax._
import fs2.Strategy
import fs2.interop.cats._
import org.scalajs.dom.Element

object Widget {

  implicit val strategy = Strategy.default

  implicit def widgetAsList(widget: Widget): Fragment = widget.map(List(_))

  implicit val widgetEventMapper: WidgetEventMapper[Widget] = new WidgetEventMapper[Widget] {
    def mapEvents(widget: Widget)(f: WidgetEvents => WidgetEvents): Widget =
      widget.map(implicitly[WidgetEventMapper[Binding]].mapEvents(_)(f))
  }

  implicit class WidgetOps(override val instance: Widget) extends WidgetEventMapperOps[Widget] {

    val mapper: WidgetEventMapper[Widget] = widgetEventMapper

    def element = instance.map(_.element)

    def mapElement(f: Element => Element): Widget = instance.map(_.mapElement(f))

    def emitDomEvents(eventTypes: Dom.Event.Type*): Widget =
      instance.map { binding =>
        val domEvents = binding.element.stream(eventTypes: _*).map(_.left)
        binding.copy(events = EventsComposer.both.compose(binding.events, domEvents))
      }

    def append(child: Binding): Widget = instance.map(_ unsafeAppend child)
    def append(widget: Widget): Widget = instance.flatMap(append)
    def appendFragment(fragment: Fragment): Widget =
      for {
        binding <- instance
        bindings <- fragment
      } yield bindings.foldLeft(binding)(_ unsafeAppend _)

    def ::(left: Widget): Fragment = (left :: instance :: Nil).sequence

    def setClass(classes: String*): Widget = instance.mapElement(_.setClass(classes: _*))
  }

  implicit class FragmentOps(val fragment: Fragment) extends AnyVal {
    def ::(widget: Widget): Fragment = for { b <- widget; bs <- fragment } yield b :: bs
    def mapAll(pf: PartialFunction[List[Binding], List[Binding]]): Fragment = fragment.map(pf.total)
  }

}
