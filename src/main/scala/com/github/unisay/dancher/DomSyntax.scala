package com.github.unisay.dancher

import cats.data.NonEmptyList
import com.github.unisay.dancher.Dom.Event.Type
import fs2.async.mutable.Queue
import fs2.{Strategy, Stream, Task, async}
import org.scalajs.dom._

import scala.scalajs.js

object DomSyntax {

  implicit class ElementSyntax(val element: Element) extends AnyVal {

    def removeClass(classes: String*): Element = {
      val cx = classes.toList
      if (cx.nonEmpty) {
        val existingClasses = Option(element.getAttribute("class")).toArray.flatMap(_.split("\\s+"))
      }
      element
    }

    def setClass(classes: String*): Element = {
      NonEmptyList
        .fromList(classes.toList)
        .foreach { nel =>
          val classList = nel ++ Option(element.getAttribute("class")).toList
          element.setAttribute("class", classList.toList.mkString(" "))
        }
      element
    }

    def appendAll(children: Seq[Element]) =
      children.foldLeft(element) { (parent, child) => parent.appendChild(child); parent }

    private def makeQueue(eventTypes: String*)(implicit S: Strategy) = {

      type EventQueue = Queue[Task, Event]
      type EventListener = js.Function1[Event, Unit]

      def clickEventListener(queue: EventQueue): EventListener = { evt: Event =>
        queue.enqueue1(evt).unsafeRunAsync(_ => ())
      }

      def createQueue = {
        for {
          queue <- async.unboundedQueue[Task, Event]
          listenerFn = clickEventListener(queue)
          _ <- Task.delay(eventTypes.foreach(element.addEventListener(_, listenerFn)))
        } yield (queue, listenerFn)
      }

      def emitQueue(queue: EventQueue, listenerFn: EventListener): Stream[Task, Event] =
        queue.dequeueAvailable

      def cleanupQueue(queue: EventQueue, listenerFn: EventListener) =
        Task.delay(eventTypes.foreach(element.removeEventListener(_, listenerFn)))

      val useQ: ((EventQueue, EventListener)) => Stream[Task, Event] = (emitQueue _).tupled
      val releaseQ: ((EventQueue, EventListener)) => Task[Unit] = (cleanupQueue _).tupled
      Stream.bracket(createQueue)(useQ, releaseQ)
    }

    def stream(eventTypes: Type*)(implicit S: Strategy): Stream[Task, Event] = makeQueue(eventTypes.map(_.name): _*)
  }
}
