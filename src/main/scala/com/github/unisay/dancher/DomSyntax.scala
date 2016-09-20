package com.github.unisay.dancher

import cats.data.NonEmptyList
import com.github.unisay.dancher.Dom.Event.Type
import fs2.async.mutable.Queue
import fs2.{Strategy, Stream, Task, async}
import org.scalajs.dom._

import scala.scalajs.js

object DomSyntax {

  implicit class ElementSyntax(val element: Element) extends AnyVal {

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

      def clickEventListener(queue: Queue[Task, Event]): js.Function1[Event, Unit] = { evt: Event =>
        queue.enqueue1(evt).unsafeRunAsync(_ => ())
      }

      def createQueue = {
        for {
          queue <- async.unboundedQueue[Task, Event]
          listenerFn = clickEventListener(queue)
          _ <- Task.delay(eventTypes.foreach(element.addEventListener(_, listenerFn)))
        } yield (queue, listenerFn)
      }

      def emitQueue(queue: Queue[Task, Event], listenerFn: js.Function1[Event, Unit]) =
        Stream.emit(queue)

      def cleanupQueue(queue: Queue[Task, Event], listenerFn: js.Function1[Event, Unit]) =
        Task.delay(eventTypes.foreach(element.removeEventListener(_, listenerFn)))

      Stream.bracket(createQueue)((emitQueue _).tupled, (cleanupQueue _).tupled)
    }

    def stream(eventTypes: Type*)(implicit S: Strategy): Stream[Task, Event] =
      makeQueue(eventTypes.map(_.name): _*).flatMap(_.dequeueAvailable)
  }
}
