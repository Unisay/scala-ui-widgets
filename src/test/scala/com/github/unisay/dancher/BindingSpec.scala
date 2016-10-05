package com.github.unisay.dancher

import com.github.unisay.dancher.Arbitraries._
import com.github.unisay.dancher.DomArbitraries._
import com.github.unisay.dancher.Equalities._
import fs2.{Stream, Task}
import org.scalajs.dom.Event
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{AsyncFlatSpec, MustMatchers, PropSpec}

class BindingSpec extends AsyncFlatSpec with MustMatchers {

  implicit override def executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  val domEvent1 = createDomEvent(1)
  val domEvent2 = createDomEvent(2)
  val domEvent3 = createDomEvent(3)
  val domainEvent1 = createDomainEvent(1)
  val domainEvent2 = createDomainEvent(2)
  val domainEvent3 = createDomainEvent(3)
  val parent = createBinding(1)
  val child = createBinding(2)
  val childWithDomEvents = child.copy(domEvents = Stream[Task, Event](domEvent1, domEvent2))
  val childWithDomainEvents = child.copy(domainEvents = Stream[Task, DomainEvent](domainEvent1, domainEvent2))

  it must "return deepElement" in {
    parent.append(child).deepElement.unsafeRunAsyncFuture() map { _ mustEqual parent.element }
  }

  it must "return deepDomEvents" in {
    val events = parent.append(childWithDomEvents).deepDomEvents
    events.runLog.unsafeRunAsyncFuture() map { _ must contain allOf(domEvent1, domEvent2) }
  }
  
  it must "return deepDomainEvents" in {
    val events = parent.append(childWithDomainEvents).deepDomainEvents
    events.runLog.unsafeRunAsyncFuture() map { _ must contain allOf(domainEvent1, domainEvent2) }
  }

  it must "map dom event" in {
    val events = childWithDomEvents.mapDomEvent(event => if (event == domEvent1) domEvent3 else event).deepDomEvents
    events.runLog.unsafeRunAsyncFuture() map { _ must (not contain domEvent1 and contain(domEvent3)) }
  }


  it must "map domain event" in {
    val events = childWithDomainEvents.mapDomainEvent(event => if (event == domainEvent1) domainEvent3 else event)
      .deepDomainEvents
    events.runLog.unsafeRunAsyncFuture() map { _ must (not contain domainEvent1 and contain(domainEvent3)) }
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
