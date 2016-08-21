package com.github.unisay.dancher

import monix.execution.schedulers.{ExecutionModel, TestScheduler}
import monix.reactive.Observable
import org.specs2.concurrent.ExecutionEnv

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

object ObservableMatchers {

  implicit class ObservableOps[A](observable: Observable[A]) {
    def toList(implicit ee: ExecutionEnv): List[A] = {
      implicit val testScheduler = TestScheduler(ExecutionModel.SynchronousExecution)
      val future: Future[List[A]] = observable.toListL.runAsync
      testScheduler.tick(365.days)
      Await.result(future, 10.seconds)
    }
  }

}
