package com.github.unisay.dancher

import com.github.unisay.dancher.Dom.Event.Type
import fs2.async.mutable.Queue
import fs2.{Strategy, Stream, Task, async}
import org.scalajs.dom._

import scala.scalajs.js

object DomSyntax {

  implicit class ElementSyntax(val element: Element) extends AnyVal {

    def setStyle(style: String): Unit = element.setAttribute("style", style)

    def cssClasses: List[String] = Option(element.getAttribute("class")).toArray.flatMap(_.split("\\s+")).toList

    def setClasses(classes: String*) =
      if (classes.nonEmpty) element.setAttribute("class", classes.mkString(" "))
      else element.removeAttribute("class")

    def removeClasses(classes: String*): Unit = setClasses(cssClasses.toSet.diff(classes.toSet).toSeq: _*)

    def addClasses(classes: String*): Unit = setClasses((cssClasses ++ classes).toSet.toSeq: _*)

    def appendAll(children: Seq[Element]) =
      children.foldLeft(element) { (parent, child) => parent.appendChild(child); parent }

    private def makeQueue(eventTypes: String*)(implicit S: Strategy) = {

      type EventQueue = Queue[Task, Event]
      type EventListener = js.Function1[Event, Unit]

      def queueToEventListener(queue: EventQueue): EventListener = { event: Event =>
        queue.enqueue1(event).unsafeRunAsync(_ => ())
      }

      def createQueue = {
        for {
          queue <- async.unboundedQueue[Task, Event]
          listenerFn = queueToEventListener(queue)
          _ <- Task.delay(eventTypes.foreach(element.addEventListener(_, listenerFn, useCapture = false)))
        } yield (queue, listenerFn)
      }

      def emitQueue(queue: EventQueue, listenerFn: EventListener): Stream[Task, Event] =
        queue.dequeueAvailable

      def cleanupQueue(queue: EventQueue, listenerFn: EventListener) =
        Task.delay(eventTypes.foreach(element.removeEventListener(_, listenerFn)))

      val useQ: ((EventQueue, EventListener)) => Stream[Task, Event] = (emitQueue _).tupled
      val releaseQ: ((EventQueue, EventListener)) => Effect = (cleanupQueue _).tupled
      Stream.bracket(createQueue)(useQ, releaseQ)
    }

    def stream(eventTypes: Type*)(implicit S: Strategy): Stream[Task, Event] = makeQueue(eventTypes.map(_.name): _*)
  }
}
