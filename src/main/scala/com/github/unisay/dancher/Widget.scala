package com.github.unisay.dancher

import cats.implicits._
import com.github.unisay.dancher.DomSyntax._
import com.github.unisay.dancher.Syntax._
import fs2.Strategy
import fs2.interop.cats._
import org.scalajs.dom.Element

import scala.language.implicitConversions

object Widget {

  implicit val strategy = Strategy.default

  implicit def widgetAsList(widget: Widget): Fragment = widget.map(List(_))

  implicit class WidgetOps(val instance: Widget) {

    def element = instance.map(_.element)
    def mapElement(f: Element => Element) = instance.map(_.mapElement(f))
    def mapDomainEvent(f: DomainEvent => DomainEvent) = instance.map(_.mapDomainEvent(f))
    def emitDomEvents(eventTypes: Dom.Event.Type*) =
      instance.map { binding =>
        binding.pipeDomEvents(_ merge binding.element.stream(eventTypes: _*))
      }

    def append(child: Binding): Widget = instance.map(_ append child)
    def append(widget: Widget): Widget = instance.flatMap(append)
    def appendFragment(f: Fragment): Widget = f.flatMap(_.foldLeft(instance)((p, c) => p.map(_ append c)))

    def ::(left: Widget): Fragment = (left :: instance :: Nil).sequence

    def setClass(classes: String*) = instance.mapElement(_.setClass(classes: _*))
  }

  implicit class FragmentOps(val fragment: Fragment) extends AnyVal {
    def ::(widget: Widget): Fragment = for { b <- widget; bs <- fragment } yield b :: bs
    def mapTotal(pf: PartialFunction[List[Binding], List[Binding]]) = fragment.map(pf.total)
  }
}
