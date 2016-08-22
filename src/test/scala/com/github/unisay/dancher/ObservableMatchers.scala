package com.github.unisay.dancher

import monix.execution.schedulers.TestScheduler
import monix.reactive.Observable

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object ObservableMatchers {

  implicit class ObservableOps[A](observable: Observable[A]) {
    def toList(after: FiniteDuration = 365.days)(implicit scheduler: TestScheduler): List[A] = {
      val future: Future[List[A]] = observable.toListL.runAsync
      scheduler.tick(after)
      Await.result(future, 1.second)
    }
  }

}
