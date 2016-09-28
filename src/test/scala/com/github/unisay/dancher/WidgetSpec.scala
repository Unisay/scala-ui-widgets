package com.github.unisay.dancher

import com.github.unisay.dancher.Arbitraries._
import com.github.unisay.dancher.Widget._
import fs2.Stream
import org.scalatest.{AsyncFlatSpec, MustMatchers}

class WidgetSpec extends AsyncFlatSpec with MustMatchers {

  implicit override def executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  val widgetEvent = sample[WidgetEvent]
  val parent = sample[Widget]
  val child = sample[Widget]

  "WidgetOps" must "appendFragment" in {
    parent.appendFragment(child.mapWidgetEvents(_ ++ Stream(widgetEvent)))
      .flatMap(_.deepEvents.runLog).unsafeRunAsyncFuture() map {
        _ must contain(widgetEvent)
      }
  }

}
