package com.github.unisay.dancher.widget

import monix.execution.schedulers.TestScheduler
import monix.reactive.OverflowStrategy
import monix.reactive.subjects.ConcurrentSubject
import org.scalatest.{FlatSpec, MustMatchers}
import scala.util.Success
import scala.concurrent.duration._

class SchedulerSpec extends FlatSpec with MustMatchers {

  "TestScheduler" must "work correctly" in {
    implicit val scheduler = TestScheduler()
    val subject = ConcurrentSubject.publish[Int](OverflowStrategy.Unbounded)

    scheduler.scheduleOnce(1.second) { subject.onNext(1); () }
    scheduler.scheduleOnce(2.second) { subject.onNext(2); () }
    scheduler.scheduleOnce(3.second) { subject.onNext(3); () }
    scheduler.scheduleOnce(4.second) { subject.onComplete()  }

    val eventualList = subject.toListL.runAsync
    scheduler.tick(10.seconds)
    eventualList.value must contain(Success(List(1, 2, 3)))
  }

}
