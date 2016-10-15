package com.github.unisay.dancher

import fs2._
import org.scalajs.dom
import org.scalajs.dom._
import org.scalatest.Assertion
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import DomArbitraries._

object TestUtils {

  implicit class TaskOps[O](task: Task[O]) {
    def assert(f: O => Assertion): Future[Assertion] = task.unsafeRunAsyncFuture().map(f)
  }

  implicit class StreamOps[O](stream: Stream[Task, O]) {
    def assertElements(n: Long)(f: Vector[O] => Assertion) = stream.take(n).runLog.assert(f)
  }

  implicit class ElementOps(element: Element) {

    def sendEvent(eventType: Dom.Event.Type): Event = {
      val event = createDomEvent(eventType)
      element.dispatchEvent(event)
      event
    }

    def click() = sendEvent(Dom.Event.Click)
  }

  def mouseEvent(element: Element, x: Number, y: Number, typeArg: String): Unit = {
    val event = document.createEvent("MouseEvents")
    val ix = x.intValue()
    val iy = y.intValue()
    println(s"Simulated mouse move: $ix:$iy")
    event.asInstanceOf[MouseEvent].initMouseEvent(
      typeArg = typeArg,
      canBubbleArg = true,
      cancelableArg = true,
      viewArg = window,
      detailArg = 0,
      screenXArg = ix,
      screenYArg = iy,
      clientXArg = ix,
      clientYArg = iy,
      ctrlKeyArg = false,
      altKeyArg = false,
      shiftKeyArg = false,
      metaKeyArg = false,
      buttonArg = 0,
      relatedTargetArg = element
    )
    element.dispatchEvent(event)
    ()
  }

  def mouseMove(element: Element, x: Number, y: Number): Unit = mouseEvent(element, x, y, typeArg = "mousemove")
  def mouseUp(element: Element, x: Number, y: Number): Unit = mouseEvent(element, x, y, typeArg = "mouseup")
  def mouseDown(element: Element, x: Number, y: Number): Unit = mouseEvent(element, x, y, typeArg = "mousedown")

  def asynchronously(r: => Any): Unit = {
    dom.window.setTimeout(() => r, 100)
    ()
  }

}
