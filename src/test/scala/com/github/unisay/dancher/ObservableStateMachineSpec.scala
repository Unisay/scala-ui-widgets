package com.github.unisay.dancher

import com.github.unisay.dancher.ObservableMatchers._
import com.github.unisay.dancher.dom.{DomEventType, MouseDown, MouseEnter, MouseLeave, MouseMove, MouseUp}
import monix.execution.schedulers.TestScheduler
import monix.reactive.OverflowStrategy
import monix.reactive.subjects.ConcurrentSubject
import org.scalatest.{FlatSpec, MustMatchers}

import scala.concurrent.duration._

class ObservableStateMachineSpec extends FlatSpec with MustMatchers {

  "state machine" must "work" in {
    implicit val scheduler = TestScheduler()

    val events = ConcurrentSubject.publish[DomEventType](OverflowStrategy.Unbounded)

    scheduler.scheduleOnce(100.millis) { events.onNext(MouseMove);  () }
    scheduler.scheduleOnce(105.millis) { events.onNext(MouseEnter); () }
    scheduler.scheduleOnce(110.millis) { events.onNext(MouseMove);  () }
    scheduler.scheduleOnce(120.millis) { events.onNext(MouseMove);  () }
    scheduler.scheduleOnce(130.millis) { events.onNext(MouseDown);  () }
    scheduler.scheduleOnce(140.millis) { events.onNext(MouseMove);  () }
    scheduler.scheduleOnce(150.millis) { events.onNext(MouseLeave); () }
    scheduler.scheduleOnce(160.millis) { events.onNext(MouseMove);  () }
    scheduler.scheduleOnce(170.millis) { events.onNext(MouseMove);  () }
    scheduler.scheduleOnce(180.millis) { events.onNext(MouseUp);    () }
    scheduler.scheduleOnce(900.millis) { events.onComplete()           }

    case class Drag(inside: Boolean, dragging: Boolean)
    val initial = Drag(inside = false, dragging = false)

    val drags = events.scan(initial) {
      case (drag @ Drag(false, _), MouseEnter) =>
        drag.copy(inside = true)
      case (drag @ Drag(true, _), MouseLeave) =>
        drag.copy(inside = false)
      case (drag @ Drag(true, false), MouseDown) =>
        drag.copy(dragging = true)
      case (drag @ Drag(_, true), MouseUp) =>
        drag.copy(dragging = false)
      case (drag, _) =>
        drag
    }

    drags.toList() must contain theSameElementsInOrderAs List(
      Drag(inside = false, dragging = false), // MouseMove
      Drag(inside = true,  dragging = false), // MouseEnter
      Drag(inside = true,  dragging = false), // MouseMove
      Drag(inside = true,  dragging = false), // MouseMove
      Drag(inside = true,  dragging = true),  // MouseDown
      Drag(inside = true,  dragging = true),  // MouseMove
      Drag(inside = false, dragging = true),  // MouseLeave
      Drag(inside = false, dragging = true),  // MouseMove
      Drag(inside = false, dragging = true),  // MouseMove
      Drag(inside = false, dragging = false)  // MouseUp
    )
  }

}
