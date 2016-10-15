package com.github.unisay.dancher.widget

import cats.instances.string._
import cats.syntax.all._
import com.github.unisay.dancher.Dom.Event._
import com.github.unisay.dancher.DomSyntax._
import com.github.unisay.dancher.Widget._
import com.github.unisay.dancher._
import com.github.unisay.dancher.widget.BasicWidgets._
import fs2.interop.cats._
import org.scalajs.dom.{Element, MouseEvent}

object LayoutWidgets {

  def verticalSplit(left: Widget, right: Widget) = split("d-split-vertical", left, right)

  def horizontalSplit(left: Widget, right: Widget) = split("d-split-horizontal", left, right)

  private def split(baseClass: String, left: Widget, right: Widget): Widget = {
    val sideClass = baseClass + "-side"
    val resizeClass = baseClass + "-resize"
    val leftHolder = div(left)
      .useElement(_.setClasses(sideClass, sideClass + "-left", "d-no-select", "d-no-drag")) // TODO: adopt not wrap
    val rightHolder = div(right)
      .useElement(_.setClasses(sideClass, sideClass + "-right", "d-no-select", "d-no-drag"))
    val edge = div()
      .useElement(_.setClasses(baseClass + "-edge", resizeClass))
      .emitDomEvents(MouseEnter, MouseLeave, MouseMove, MouseUp, MouseDown)

    def screen(mouseEvent: MouseEvent) = Point(mouseEvent.screenX, mouseEvent.screenY)
    case class Drag(inside: Boolean,
                    initWidth: Option[Int] = None,
                    start: Option[Point] = None,
                    current: Option[Point] = None,
                    effect: Option[Effect] = None)

    // TODO: disable selection and cursor during drag

    def getWidth(drag: Drag): Option[Long] = {
      for {
        start <- drag.start
        curr <- drag.current
        width <- drag.initWidth
      } yield Math.max(Math.round(width - start.x + curr.x), 1)
    }

    def resize(element: Element)(width: Long) = Effect(element.setStyle(s"width: ${width}px"))
    def setResizeCursor(element: Element) = Effect(element.addClasses(resizeClass))
    def unsetResizeCursor(element: Element) = Effect(element.removeClasses(resizeClass))

    div(leftHolder :: edge :: rightHolder)
      .useElement(_.setClasses(baseClass))
      .emitDomEvents(MouseMove, MouseUp, MouseDown, MouseLeave)
      .map { binding =>
        val element = binding.nested.head.element
        binding.pipeDomEvents {
          _.scan(Drag(inside = false)) {

            case (drag, (event: MouseEvent))
              if drag.start.isDefined && event.`type` === MouseMove.name =>
              drag.copy(current = screen(event).some, effect = None)

            case (drag, (event: MouseEvent))
              if !drag.inside && event.`type` === MouseEnter.name =>
              drag.copy(inside = true, current = screen(event).some, effect = None)

            case (drag, (event: MouseEvent))
              if drag.inside && event.`type` === MouseLeave.name =>
              drag.copy(inside = false, current = screen(event).some, effect = None)

            case (drag, (event: MouseEvent))
              if !drag.inside && event.`type` === MouseLeave.name =>
              drag.copy(
                start = None,
                current = screen(event).some,
                effect = unsetResizeCursor(binding.element).some)

            case (drag, (event: MouseEvent))
              if drag.inside && drag.start.isEmpty && event.`type` === MouseDown.name =>
              drag.copy(
                start = screen(event).some,
                current = screen(event).some,
                initWidth = element.clientWidth.some,
                effect = setResizeCursor(binding.element).some)

            case (drag, (event: MouseEvent))
              if drag.start.isDefined && event.`type` === MouseUp.name =>
              drag.copy(
                start = None,
                current = screen(event).some,
                effect = unsetResizeCursor(binding.element).some)

            case (drag, _) if drag.effect.isDefined =>
              drag.copy(effect = None)

            case (drag, _) =>
              drag
          }
          .evalMap { drag =>
            val resizeEffect = getWidth(drag).fold(NoEffect)(resize(element))
            val otherEffect = drag.effect getOrElse NoEffect
            otherEffect >> resizeEffect
          }
          .drain
        }
      }

  }

}
