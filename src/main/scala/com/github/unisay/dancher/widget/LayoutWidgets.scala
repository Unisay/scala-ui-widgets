package com.github.unisay.dancher.widget

import cats.data.Xor._
import cats.instances.string._
import cats.syntax.eq._
import com.github.unisay.dancher.Dom.Event._
import com.github.unisay.dancher.Widget._
import com.github.unisay.dancher._
import com.github.unisay.dancher.widget.BasicWidgets._
import com.outr.scribe.Logging
import fs2.Task
import org.scalajs.dom.MouseEvent

object LayoutWidgets extends Logging {

  def verticalSplit(left: Widget, right: Widget) = split("d-split-vertical", left, right)

  def horizontalSplit(left: Widget, right: Widget) = split("d-split-horizontal", left, right)

  private def split(baseClass: String, left: Widget, right: Widget): Widget = {
    val sideClass = baseClass + "-side"
    val leftHolder = div(left).setClass(sideClass, sideClass + "-left")
    val rightHolder = div(right).setClass(sideClass, sideClass + "-right")
    val edge = div.setClass(baseClass + "-edge").emitDomEvents(MouseEnter, MouseLeave, MouseMove, MouseUp, MouseDown)

    def screen(mouseEvent: MouseEvent): Some[Point] = Some(Point(mouseEvent.screenX, mouseEvent.screenY))
    case class Drag(inside: Boolean,
                    initWidth: Option[Int] = None,
                    start: Option[Point] = None,
                    current: Option[Point] = None,
                    end: Option[Point] = None)

    div(leftHolder :: edge :: rightHolder)
      .setClass(baseClass)
      .emitDomEvents(MouseMove, MouseUp, MouseDown)
      .map { binding =>
        val element = binding.nested.head.element
        binding.mapEvents {
          _.scan(Drag(inside = false)) {
            case (drag, Left(event: MouseEvent)) if drag.start.isDefined && event.`type` === MouseMove.name =>
              logger.debug(drag)
              drag.copy(current = screen(event))
            case (drag, Left(event: MouseEvent)) if !drag.inside && event.`type` === MouseEnter.name =>
              logger.debug(drag)
              drag.copy(inside = true, current = screen(event))
            case (drag, Left(event: MouseEvent)) if drag.inside && event.`type` === MouseLeave.name =>
              logger.debug(drag)
              drag.copy(inside = false, current = screen(event))
            case (drag, Left(event: MouseEvent)) if drag.inside && drag.start.isEmpty && event.`type` === MouseDown.name =>
              logger.debug(drag)
              drag.copy(initWidth = Some(element.clientWidth), start = screen(event), current = screen(event))
            case (drag, Left(event: MouseEvent)) if drag.start.isDefined && event.`type` === MouseUp.name =>
              logger.debug(drag)
              drag.copy(end = screen(event), current = screen(event))
            case (drag, _) if drag.start.isDefined && drag.end.isDefined =>
              drag.copy(start = None, end = None)
            case (drag, _) =>
              drag
          }
          .filter(drag => drag.start.isDefined && drag.end.isEmpty)
          .map { drag =>
            for {
              start <- drag.start
              curr  <- drag.current
              width <- drag.initWidth
            } yield Math.max(Math.round(width - start.x + curr.x), 1)
          }
          .filter(_.isDefined)
          .evalMap(width => Task.delay(element.setAttribute("style", s"width: ${width}px")))
          .drain
        }
      }

  }

}
