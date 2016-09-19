package com.github.unisay.dancher

import cats.data.NonEmptyList
import fs2.async.mutable.Queue
import fs2.{Strategy, Stream, Task, async}
import org.scalajs.dom._

import scala.scalajs.js

object DomSyntax {

  implicit class ElementSyntax(val element: Element) extends AnyVal {

    def setClasses(classes: String*): Element = {
      NonEmptyList
        .fromList(classes.toList)
        .foreach { nel =>
          val classList = nel ++ Option(element.getAttribute("class")).toList
          element.setAttribute("class", classList.toList.mkString(" "))
        }
      element
    }

    def setClass(cssClass: String): Element = setClasses(cssClass)

    def appendAll(children: Seq[Element]) =
      children.foldLeft(element) { (parent, child) => parent.appendChild(child); parent }

    private def makeQueue(eventType: String)(implicit S: Strategy) = {

      def clickEventListener(queue: Queue[Task, Event]): js.Function1[Event, Unit] = { evt: Event =>
        queue.enqueue1(evt).unsafeRunAsync(_ => ())
      }

      def createQueue = {
        for {
          queue <- async.unboundedQueue[Task, Event]
          listenerFn = clickEventListener(queue)
          _ <- Task.delay(element.addEventListener(eventType, listenerFn))
        } yield (queue, listenerFn)
      }

      def emitQueue(queue: Queue[Task, Event], listenerFn: js.Function1[Event, Unit]) =
        Stream.emit(queue)

      def cleanupQueue(queue: Queue[Task, Event], listenerFn: js.Function1[Event, Unit]) =
        Task.delay(element.removeEventListener(eventType, listenerFn))

      Stream.bracket(createQueue)((emitQueue _).tupled, (cleanupQueue _).tupled)
    }

    def stream(eventType: String)(implicit S: Strategy): Stream[Task, Event] =
      makeQueue(eventType).flatMap(_.dequeueAvailable)
  }
}
