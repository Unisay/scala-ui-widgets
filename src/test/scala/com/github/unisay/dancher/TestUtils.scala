package com.github.unisay.dancher

import fs2._
import org.scalajs.dom
import org.scalatest.Assertion
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object TestUtils {

  implicit class TaskOps[O](task: Task[O]) {
    def assert(f: O => Assertion): Future[Assertion] = task.unsafeRunAsyncFuture().map(f)
  }

  implicit class StreamOps[O](stream: Stream[Task, O]) {
    def assertElements(n: Long)(f: Vector[O] => Assertion) = stream.take(n).runLog.assert(f)
  }

  def asynchronously(r: => Any): Unit = {
    dom.window.setTimeout(() => r, 100)
    ()
  }

}
