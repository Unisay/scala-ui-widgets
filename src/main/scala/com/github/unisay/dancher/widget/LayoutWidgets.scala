package com.github.unisay.dancher.widget

import cats.Eval
import com.github.unisay.dancher.Dom.Event._
import com.github.unisay.dancher.Fragment._
import com.github.unisay.dancher.Widget._
import com.github.unisay.dancher._
import com.github.unisay.dancher.widget.BasicWidgets._
import fs2.{Stream, Pipe, Task}
import org.scalajs.dom.{Event, MouseEvent}
import cats.instances.string._
import cats.syntax.eq._
import com.outr.scribe.Logging

object LayoutWidgets extends Logging {

  def verticalSplit(left: Widget, right: Widget) = split("d-split-vertical", left, right)

  def horizontalSplit(left: Widget, right: Widget) = split("d-split-horizontal", left, right)

  private def split(baseClass: String, left: Fragment, right: Fragment): Widget = {
    case class Drag[S](inside: Boolean, state: Eval[S],
                       start: Option[Point] = None, current: Option[Point] = None, end: Option[Point] = None)
    def screen(mouseEvent: MouseEvent): Some[Point] = Some(Point(mouseEvent.screenX, mouseEvent.screenY))
    def splitterPipe[S](initialDrag: Drag[S]): Pipe[Task, Event, DomainEvent] =
      _.scan(initialDrag) {
        case (drag@Drag(_, _, Some(_), _, _), event: MouseEvent) if event.`type` === MouseMove.name =>
          logger.debug(drag)
          drag.copy(current = screen(event))
        case (drag@Drag(false,_,  _, _, _), event: MouseEvent) if event.`type` === MouseEnter.name =>
          logger.debug(drag)
          drag.copy(inside = true, current = screen(event))
        case (drag@Drag(true,_,  _, _, _), event: MouseEvent) if event.`type` === MouseLeave.name =>
          logger.debug(drag)
          drag.copy(inside = false, current = screen(event))
        case (drag@Drag(true,_,  None, _, _), event: MouseEvent) if event.`type` === MouseDown.name =>
          logger.debug(drag)
          drag.copy(start = screen(event), current = screen(event))
        case (drag@Drag(_,_,  Some(_), _, _), event: MouseEvent) if event.`type` === MouseUp.name =>
          logger.debug(drag)
          drag.copy(end = screen(event), current = screen(event))
        case (drag@Drag(_, _, Some(_), _, Some(_)), _) =>
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
      .map(width => setAttribute("style", "width: " + width.px)(element).void)

    val widgetPipe: Pipe[Task, Event, DomainEvent] = ???

    val sideClass = baseClass + "-side"

    val leftHolder = div(left).setClass(sideClass, sideClass + "-left")
    val rightHolder = div(right).setClass(sideClass, sideClass + "-right")
    val initialDrag = Drag(inside = false, state = Eval.True) // TODO: what if inside is true?
    val splitter = div
      .setClass(baseClass + "-handle")
      .pipeDomEvents(MouseEnter, MouseLeave, MouseMove, MouseUp, MouseDown)(splitterPipe(initialDrag))

    div(leftHolder <*> splitter <*> rightHolder)
      .setClass(baseClass)
      .pipeDomEvents(MouseMove, MouseUp, MouseDown)(widgetPipe)
  }

}
