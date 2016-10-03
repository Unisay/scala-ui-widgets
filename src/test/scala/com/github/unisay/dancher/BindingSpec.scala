package com.github.unisay.dancher

import com.github.unisay.dancher.Arbitraries._
import com.github.unisay.dancher.Equalities._
import fs2.{Stream, Task}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{AsyncFlatSpec, MustMatchers, PropSpec}

class BindingSpec extends AsyncFlatSpec with MustMatchers {

  implicit override def executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  val widgetEvent1 = sample[WidgetEvent] // TODO generate unique events
  val widgetEvent2 = sample[WidgetEvent]
  val widgetEvent3 = sample[WidgetEvent]
  val testEvents = Stream[Task, WidgetEvent](widgetEvent1, widgetEvent2)
  val parent = sample[Binding]
  val child = sample[Binding]

  it must "return deepElement" in {
    parent.append(child).deepElement.unsafeRunAsyncFuture() map { _ mustEqual parent.element }
  }

  it must "return deepEvents" in {
    val childWithEvent = child.copy(events = testEvents)
    val deepEvents = parent.append(childWithEvent).deepDomEvents
    deepEvents.runLog.unsafeRunAsyncFuture() map { _ must contain allOf(widgetEvent1, widgetEvent2) }
  }

  it must "map widget event" in {
    val childWithEvent = child
      .copy(events = testEvents)
      .mapWidgetEvent(widgetEvent => if (widgetEvent == widgetEvent1) widgetEvent3 else widgetEvent)

    childWithEvent.deepDomEvents.runLog.unsafeRunAsyncFuture() map { events =>
      events must (not contain widgetEvent1 and contain(widgetEvent3))
    }
  }

}

class BindingPropSpec extends PropSpec with MustMatchers with GeneratorDrivenPropertyChecks {

  property("Append returns parent element") {
    forAll { (parent: Binding, child: Binding) =>
      parent.append(child).element mustEqual parent.element
    }
  }

  property("Append adds child to nested bindings") {
    forAll { (parent: Binding, child: Binding) =>
      parent.append(child).nested must contain(child)
    }
  }

}
