package com.github.unisay.dancher

import com.github.unisay.dancher.Arbitraries._
import com.github.unisay.dancher.DomArbitraries._
import com.github.unisay.dancher.Equalities._
import com.github.unisay.dancher.Widget._
import fs2.{Stream, Task}
import org.scalajs.dom._
import org.scalatest.{AsyncFlatSpec, MustMatchers}

class WidgetSpec extends AsyncFlatSpec with MustMatchers {

  implicit override def executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  val child = createWidget(2)
  val parent = createWidget(1)
  val domEvent1 = createDomEvent(1)
  val domEvent2 = createDomEvent(2)
  val childWithDomEvents = child.map(_.copy(domEvents = Stream[Task, Event](domEvent1, domEvent2)))

  "WidgetOps" must "appendFragment" in {
    parent.appendFragment(childWithDomEvents).flatMap(_.deepDomEvents.runLog).unsafeRunAsyncFuture() map
      { _ must contain allOf(domEvent1, domEvent2) }
  }

}
