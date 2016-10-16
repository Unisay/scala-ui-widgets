package com.github.unisay.dancher.widget

import cats.instances.string._
import cats.syntax.all._
import com.github.unisay.dancher.Dom.Event._
import com.github.unisay.dancher.DomSyntax._
import com.github.unisay.dancher.Widget._
import com.github.unisay.dancher._
import com.github.unisay.dancher.widget.BasicWidgets._
import fs2.Task.delay
import fs2._
import fs2.interop.cats._
import org.scalajs.dom.{Element, MouseEvent}

object LayoutWidgets {

  def verticalSplit(left: Widget, right: Widget) = split("d-split-vertical", left, right)

  def horizontalSplit(left: Widget, right: Widget) = split("d-split-horizontal", left, right)

  case class SplitResized(binding: Binding, leftWidth: Int) extends DomainEvent

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
    object EmptyEvent extends DomainEvent
    val NoAction: Task[DomainEvent] = Task.now(EmptyEvent)
    case class Drag(inside: Boolean,
                    initWidth: Option[Int] = None,
                    start: Option[Point] = None,
                    current: Option[Point] = None,
                    action: Task[DomainEvent] = NoAction)

    def calcWidth(start: Point, curr: Point, initialWidth: Int): Int =
      (curr.x + initialWidth - start.x).round.max(1).toInt

    def resize(element: Element, width: Int) =
      delay { println(s"resize: $width"); element.setStyle(s"width: ${width}px"); width }

    def setResizeCursor(element: Element) =
      delay { println(s"set cursor"); element.addClasses(resizeClass); EmptyEvent }

    def unsetResizeCursor(element: Element) =
      delay { println(s"unset cursor"); element.removeClasses(resizeClass); EmptyEvent }

    div(leftHolder :: edge :: rightHolder)
      .useElement(_.setClasses(baseClass))
      .emitDomEvents(MouseMove, MouseUp, MouseDown, MouseLeave)
      .map { binding =>
        val element = binding.nested.head.element
        binding.handleDomEvents { _.through(StreamUtils.logIt("de"))
          .scan(Drag(inside = false)) {

            case (drag, (event: MouseEvent))
              if drag.start.isDefined && event.`type` === MouseMove.name =>
              val current = screen(event)
              val width = calcWidth(drag.start.get, current, drag.initWidth.get)
              val action = resize(element, width).as(EmptyEvent)
              drag.copy(current = current.some, action = action)

            case (drag, (event: MouseEvent))
              if !drag.inside && event.`type` === MouseEnter.name =>
              drag.copy(inside = true, current = screen(event).some, action = NoAction)

            case (drag, (event: MouseEvent))
              if drag.inside && event.`type` === MouseLeave.name =>
              drag.copy(inside = false, current = screen(event).some, action = NoAction)

            case (drag, (event: MouseEvent))
              if !drag.inside && event.`type` === MouseLeave.name =>
              drag.copy(start = None, current = screen(event).some, action = unsetResizeCursor(binding.element))

            case (drag, (event: MouseEvent))
              if drag.inside && drag.start.isEmpty && event.`type` === MouseDown.name =>
              drag.copy(
                start = screen(event).some,
                current = screen(event).some,
                initWidth = element.clientWidth.some,
                action = setResizeCursor(binding.element))

            case (drag, (event: MouseEvent))
              if drag.start.isDefined && event.`type` === MouseUp.name =>
              val current = screen(event)
              val width = calcWidth(drag.start.get, current, drag.initWidth.get)
              val action = unsetResizeCursor(binding.element) >> resize(element, width).as(SplitResized(binding, width))
              drag.copy(start = None, current = current.some, action = action)

            case (drag, _) => drag.copy(action = NoAction)
          }
          .evalMap(_.action)
          .filter(_ != EmptyEvent)
        }
      }

  }

}
