package com.github.unisay.dancher

import com.github.unisay.dancher.ObservableMatchers._
import com.github.unisay.dancher.dom._
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

    case class Drag(inside: Boolean, dragging: Boolean, draggingEnd: Boolean, event: Option[DomEventType])
    val initial = Drag(inside = false, dragging = false, draggingEnd = false, event = None)

    val drags = events.scan(initial) {
      case (drag @ Drag(_, _, _, _), MouseMove) =>
        drag.copy(event = Some(MouseMove))
      case (drag @ Drag(false, _, _, _), MouseEnter) =>
        drag.copy(inside = true, event = Some(MouseEnter))
      case (drag @ Drag(true, _, _, _), MouseLeave) =>
        drag.copy(inside = false, event = Some(MouseLeave))
      case (drag @ Drag(true, false, _, _), MouseDown ) =>
        drag.copy(dragging = true, event = Some(MouseDown))
      case (drag @ Drag(_, true, _, _), MouseUp) =>
        drag.copy(dragging = false, draggingEnd = true, event = Some(MouseUp))
      case (drag @ Drag(_, false, true, _), _) =>
        drag.copy(draggingEnd = false)
      case (drag, _) =>
        drag
    }.filter(drag => drag.dragging || drag.draggingEnd).map(_.event.get)

    drags.toList() must contain theSameElementsInOrderAs List(
      MouseDown,
      MouseMove,
      MouseLeave,
      MouseMove,
      MouseMove,
      MouseUp
    )
  }

}
