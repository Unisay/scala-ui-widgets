package com.github.unisay.dancher.widget

import cats.instances.string._
import cats.syntax.eq._
import com.github.unisay.dancher.Dom.Event._
import com.github.unisay.dancher.Widget._
import com.github.unisay.dancher._
import com.github.unisay.dancher.widget.BasicWidgets._
import fs2.Task
import org.scalajs.dom.MouseEvent

object LayoutWidgets {

  def verticalSplit(left: Widget, right: Widget) = split("d-split-vertical", left, right)

  def horizontalSplit(left: Widget, right: Widget) = split("d-split-horizontal", left, right)

  private def split(baseClass: String, left: Widget, right: Widget): Widget = {
    val sideClass = baseClass + "-side"
    val leftHolder = div(left).setClass(sideClass, sideClass + "-left", "d-no-select", "d-no-drag") // TODO: adopt not wrap
    val rightHolder = div(right).setClass(sideClass, sideClass + "-right", "d-no-select", "d-no-drag")
    val edge = div().setClass(baseClass + "-edge").emitDomEvents(MouseEnter, MouseLeave, MouseMove, MouseUp, MouseDown)

    def screen(mouseEvent: MouseEvent): Some[Point] = Some(Point(mouseEvent.screenX, mouseEvent.screenY))
    case class Drag(inside: Boolean,
                    initWidth: Option[Int] = None,
                    start: Option[Point] = None,
                    current: Option[Point] = None)

    // TODO: disable selection and cursor during drag

    div(leftHolder :: edge :: rightHolder)
      .setClass(baseClass)
      .emitDomEvents(MouseMove, MouseUp, MouseDown, MouseLeave)
      .map { binding =>
        val element = binding.nested.head.element
        binding.pipeDomEvents {
          _.scan(Drag(inside = false)) {
            case (drag, (event: MouseEvent)) if drag.start.isDefined && event.`type` === MouseMove.name =>
//              println("move: " + drag)
              drag.copy(current = screen(event))
            case (drag, (event: MouseEvent)) if !drag.inside && event.`type` === MouseEnter.name =>
//              println("enter: " + drag)
              drag.copy(inside = true, current = screen(event))
            case (drag, (event: MouseEvent)) if drag.inside && event.`type` === MouseLeave.name =>
//              println("leave edge: " + drag)
              drag.copy(inside = false, current = screen(event))
            case (drag, (event: MouseEvent)) if !drag.inside && event.`type` === MouseLeave.name =>
//              println("leave div: " + drag)
              drag.copy(start = None, current = screen(event))
            case (drag, (event: MouseEvent)) if drag.inside && drag.start.isEmpty && event.`type` === MouseDown.name =>
//              println("down: " + drag)
              drag.copy(initWidth = Some(element.clientWidth), start = screen(event), current = screen(event))
            case (drag, (event: MouseEvent)) if drag.start.isDefined && event.`type` === MouseUp.name =>
//              println("up: " + drag)
              drag.copy(start = None, current = screen(event))
            case (drag, _) =>
//              println(drag)
              drag
          }
          .filter(drag => drag.start.isDefined)
          .map { drag =>
            for {
              start <- drag.start
              curr  <- drag.current
              width <- drag.initWidth
            } yield Math.max(Math.round(width - start.x + curr.x), 1)
          }
          .evalMap {
            case Some(width) => Task.delay(element.setAttribute("style", s"width: ${width}px"))
            case None => Task.now(())
          }
          .drain
        }
      }

  }

}
