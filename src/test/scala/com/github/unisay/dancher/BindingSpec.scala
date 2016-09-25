package com.github.unisay.dancher

import com.github.unisay.dancher.Arbitraries._
import com.github.unisay.dancher.Equalities._
import fs2.{Strategy, Stream, Task}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.time._
import org.scalatest.{MustMatchers, PropSpec}

class BindingSpec extends PropSpec with GeneratorDrivenPropertyChecks with MustMatchers with ScalaFutures {

  implicit val defaultPatience = PatienceConfig(timeout = Span(1, Seconds), interval = Span(50, Millis))

  property("test merge") {
    forAll { (xs: List[Int], ys: List[Int]) =>
      implicit val strategy = Strategy.default
      val stream = Stream[Task, Int](xs: _*) merge Stream[Task, Int](ys: _*)
      stream.runLog.unsafeRunAsyncFuture().futureValue must not be empty
    }
  }

  property("element after append") {
    forAll { (parent: Binding, child: Binding) =>
      parent.append(child).element mustEqual parent.element
    }
  }

  property("deepElement after append") {
    forAll { (parent: Binding, child: Binding) =>
      parent.append(child).deepElement.unsafeRunAsyncFuture().futureValue mustEqual parent.element
    }
  }

  property("deepEvents after append") {
    forAll { (parent: Binding, child: Binding, widgetEvent: WidgetEvent) =>
      val testEvents = Stream[Task, WidgetEvent](widgetEvent)
      val childWithEvent = child.copy(events = testEvents)
      val deepEvents = parent.append(childWithEvent).deepEvents
      whenReady((testEvents).runLog.unsafeRunAsyncFuture()) { _ must contain(widgetEvent) }
    }
  }

  property("append") {
    forAll { (parent: Binding, child: Binding) => parent.append(child).nested must contain(child) }
  }

}
