package com.github.unisay.dancher

import com.github.unisay.dancher.DomArbitraries._
import com.github.unisay.dancher.Widget._
import fs2._
import org.scalajs.dom
import org.scalajs.dom._
import org.scalatest.Assertion

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Try

object TestUtils {

  implicit class TaskOps[O](task: Task[O]) {
    def assert(f: O => Assertion)(implicit ec: ExecutionContext): Future[Assertion] =
      task.unsafeRunAsyncFuture().map(f)
  }

  implicit class StreamOps[O](stream: Stream[Task, O]) {
    def assertElements(n: Long)(f: Vector[O] => Assertion)(implicit ec: ExecutionContext) =
      stream.take(n).runLog.assert(f)
  }

  implicit class ElementOps(element: Element) {

    def sendEvent(eventType: Dom.Event.Type): Event = {
      val event = createDomEvent(eventType)
      element.dispatchEvent(event)
      event
    }

    def removeAllChildren(): Unit = {
      var optionalChild = Option(element.lastChild)
      while(optionalChild.isDefined) {
        optionalChild.foreach(child => element.removeChild(child))
        optionalChild = Option(element.lastChild)
      }
    }

    def click() = sendEvent(Dom.Event.Click)
  }

  def mouseEvent(element: Element, x: Number, y: Number, typeArg: String): Unit = {
    val event = document.createEvent("MouseEvents").asInstanceOf[MouseEvent]
    val ix = x.intValue()
    val iy = y.intValue()
    println(s"Simulated mouse event ($typeArg): $ix:$iy")
    event.initMouseEvent(
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
      relatedTargetArg = null
    )
    element.dispatchEvent(event)
    ()
  }

  def mouseMove(element: Element, x: Number, y: Number): Unit = mouseEvent(element, x, y, typeArg = "mousemove")
  def mouseUp(element: Element, x: Number, y: Number): Unit = mouseEvent(element, x, y, typeArg = "mouseup")
  def mouseDown(element: Element, x: Number, y: Number): Unit = mouseEvent(element, x, y, typeArg = "mousedown")
  def mouseLeave(element: Element, x: Number, y: Number): Unit = mouseEvent(element, x, y, typeArg = "mouseleave")
  def mouseEnter(element: Element, x: Number, y: Number): Unit = mouseEvent(element, x, y, typeArg = "mouseenter")

  def asynchronously[A](a: => A): Future[A] = {
    val p = Promise[A]
    dom.window setTimeout(() => p.complete(Try(a)), 500)
    p.future
  }

}
