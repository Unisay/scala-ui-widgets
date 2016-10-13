package com.github.unisay.dancher.widget

import cats.instances.string._
import cats.syntax.all._
import com.github.unisay.dancher.Dom.Event._
import com.github.unisay.dancher.DomSyntax._
import com.github.unisay.dancher.Widget._
import com.github.unisay.dancher._
import com.github.unisay.dancher.widget.BasicWidgets._
import fs2.Task
import fs2.interop.cats._
import org.scalajs.dom.{Element, MouseEvent}

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
                    current: Option[Point] = None,
                    task: Option[Task[Unit]] = None)

    // TODO: disable selection and cursor during drag

    def getWidth(drag: Drag): Option[Long] = {
      for {
        start <- drag.start
        curr <- drag.current
        width <- drag.initWidth
      } yield Math.max(Math.round(width - start.x + curr.x), 1)
    }

    def updateWidth(element: Element)(width: Long): Task[Unit] =
      Task.delay(element.setAttribute("style", s"width: ${width}px")).void

    def setResizeCursor(element: Element): Task[Unit] =
      Task.delay(element.setClass("d-hor-resize")).void

    def unsetResizeCursor(element: Element): Task[Unit] =
      Task.delay(element.removeClass("d-hor-resize")).void

    div(leftHolder :: edge :: rightHolder)
      .setClass(baseClass)
      .emitDomEvents(MouseMove, MouseUp, MouseDown, MouseLeave)
      .map { binding =>
        val element = binding.nested.head.element
        binding.pipeDomEvents {
          _.scan(Drag(inside = false)) {

            case (drag, (event: MouseEvent))
              if drag.start.isDefined && event.`type` === MouseMove.name =>
              println(drag)
              drag.copy(current = screen(event), task = None)

            case (drag, (event: MouseEvent))
              if !drag.inside && event.`type` === MouseEnter.name =>
              println(drag)
              drag.copy(inside = true, current = screen(event), task = None)

            case (drag, (event: MouseEvent))
              if drag.inside && event.`type` === MouseLeave.name =>
              println(drag)
              drag.copy(inside = false, current = screen(event), task = None)

            case (drag, (event: MouseEvent))
              if !drag.inside && event.`type` === MouseLeave.name =>
              println(drag)
              drag.copy(
                start = None,
                current = screen(event),
                task = Some(unsetResizeCursor(binding.element)))

            case (drag, (event: MouseEvent))
              if drag.inside && drag.start.isEmpty && event.`type` === MouseDown.name =>
              println(drag)
              drag.copy(
                start = screen(event),
                current = screen(event),
                initWidth = Some(element.clientWidth),
                task = Some(setResizeCursor(binding.element)))

            case (drag, (event: MouseEvent))
              if drag.start.isDefined && event.`type` === MouseUp.name =>
              println(drag)
              drag.copy(
                start = None,
                current = screen(event),
                task = Some(unsetResizeCursor(binding.element)))

            case (drag, _) if drag.task.isDefined =>
              println(drag)
              drag.copy(task = None)

            case (drag, _) =>
              drag
          }
          .evalMap { drag => {
              for { task1 <- drag.task
                    task2 <- getWidth(drag) map updateWidth(binding.element)
              } yield task1 >> task2
            } orElse drag.task getOrElse Task.now(())
          }
          .drain
        }
      }

  }

}
