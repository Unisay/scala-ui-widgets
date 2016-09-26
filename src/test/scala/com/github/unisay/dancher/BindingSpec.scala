package com.github.unisay.dancher

import com.github.unisay.dancher.Arbitraries._
import com.github.unisay.dancher.Equalities._
import fs2.{Stream, Task}
import org.scalacheck.Arbitrary
import org.scalatest.{AsyncFlatSpec, MustMatchers}

class BindingSpec extends AsyncFlatSpec with MustMatchers {

  implicit override def executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  val arbitrary = Arbitrary.arbitrary[Binding]
  val widgetEvent = Arbitrary.arbitrary[WidgetEvent].sample.get
  val parent = arbitrary.sample.get
  val child = arbitrary.sample.get

  "Append" must "return parent element" in {
    parent.append(child).element mustEqual parent.element
  }

  it must "return deepElement" in {
    parent.append(child).deepElement.unsafeRunAsyncFuture() map { _ mustEqual parent.element }
  }

  it must "return deepEvents" in {
    val testEvents = Stream[Task, WidgetEvent](widgetEvent)
    val childWithEvent = child.copy(events = testEvents)
    val deepEvents = parent.append(childWithEvent).deepEvents
    deepEvents.runLog.unsafeRunAsyncFuture() map { _ must contain(widgetEvent) }
  }

  it must "append" in {
    parent.append(child).nested must contain(child)
  }

}
