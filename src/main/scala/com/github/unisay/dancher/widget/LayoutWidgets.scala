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
import org.scalajs.dom.{Element, Event, MouseEvent}

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

    def stop(mouseEvent: MouseEvent) = Effect(mouseEvent.stopImmediatePropagation())

    def screen(mouseEvent: MouseEvent) = Point(mouseEvent.screenX, mouseEvent.screenY)

    def calcWidth(start: Point, curr: Point, width: Int): Int = (curr.x + width - start.x).round.max(1).toInt

    def resize(element: Element, width: Int) = delay { element.setStyle(s"width: ${width}px"); width }

    def setResizeCursor(element: Element) = Effect(element.addClasses(resizeClass))

    def unsetResizeCursor(element: Element) = Effect(element.removeClasses(resizeClass))

    val NoAction: Task[Option[Int]] = Task.now(None)

    case class Drag(inside: Boolean,
                    initWidth: Option[Int] = None,
                    start: Option[Point] = None,
                    current: Option[Point] = None,
                    action: Task[Option[Int]] = NoAction)

    def dragIt(cursorTarget: Element, resizee: Element): (Drag, Event) => Drag = {

      case (drag, (event: MouseEvent))
        if drag.start.isDefined && event.`type` === MouseMove.name =>
        println("--MouseMove " + drag)
        val current = screen(event)
        val width = calcWidth(drag.start.get, current, drag.initWidth.get)
        val action = stop(event) >> resize(resizee, width).as(None)
        drag.copy(current = current.some, action = action)

      case (drag, (event: MouseEvent))
        if !drag.inside && event.`type` === MouseEnter.name =>
        println("--MouseEnter " + drag)
        drag.copy(inside = true, current = screen(event).some, action = NoAction)

      case (drag, (event: MouseEvent))
        if drag.inside && event.`type` === MouseLeave.name =>
        println("--MouseLeave " + drag)
        drag.copy(inside = false, current = screen(event).some, action = NoAction)

      case (drag, (event: MouseEvent))
        if !drag.inside && event.`type` === MouseLeave.name =>
        println("--MouseLeave " + drag)
        drag.copy(start = None, current = screen(event).some, action = unsetResizeCursor(cursorTarget).as(None))

      case (drag, (event: MouseEvent))
        if drag.inside && drag.start.isEmpty && event.`type` === MouseDown.name =>
        println("--MouseDown " + drag)
        drag.copy(
          start = screen(event).some,
          current = screen(event).some,
          initWidth = resizee.clientWidth.some,
          action = stop(event) >> setResizeCursor(resizee).as(None))

      case (drag, (event: MouseEvent))
        if drag.start.isDefined && event.`type` === MouseUp.name =>
        println("--MouseUp " + drag)
        val current = screen(event)
        val width = calcWidth(drag.start.get, current, drag.initWidth.get)
        val action = stop(event) >> unsetResizeCursor(resizee) >> resize(resizee, width).map(_.some)
        drag.copy(start = None, current = current.some, action = action)

      case (drag, event) =>
        println("--" + event.`type` + ": Doesn't match " + drag)
        drag.copy(action = NoAction)
    }

    div(leftHolder :: edge :: rightHolder)
      .useElement(_.setClasses(baseClass))
      .emitDomEvents(MouseMove, MouseUp, MouseDown, MouseLeave)
      .map { binding =>
        binding
        .handleDomEvents { _
          .map { domEvent =>
            val e: MouseEvent = domEvent.asInstanceOf[MouseEvent]
            println(
              s"${e.`type`}:bubbles ${e.bubbles}, target ${e.target}, phase ${e.eventPhase}, " +
              s"trusted ${e.isTrusted}, cx ${e.clientX} cy ${e.clientY}, rt ${e.relatedTarget}")
            domEvent
          }
          .scan(Drag(inside = false))(dragIt(cursorTarget = binding.element, resizee = binding.nested.head.element))
          .evalMap(_.action)
          .collect { case Some(width) => SplitResized(binding, width) }
        }
      }

  }

}
