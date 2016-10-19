package com.github.unisay.dancher

import com.github.unisay.dancher.Arbitraries._
import com.github.unisay.dancher.DomArbitraries._
import com.github.unisay.dancher.Equalities._
import com.github.unisay.dancher.TestUtils._
import fs2.{Stream, Task}
import org.scalajs.dom.Event
import org.scalatest.{AsyncFlatSpec, MustMatchers}

class BindingSpec extends AsyncFlatSpec with MustMatchers {

  implicit override def executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  val domEvent1 = createDomEvent(Dom.Event.Click)
  val domEvent2 = createDomEvent(Dom.Event.MouseDown)
  val domEvent3 = createDomEvent(Dom.Event.MouseMove)
  val domainEvent1 = createDomainEvent(1)
  val domainEvent2 = createDomainEvent(2)
  val domainEvent3 = createDomainEvent(3)
  val parent = createBinding("div")
  val child = createBinding("div")
  val childWithDomEvents = child.copy(domEvents = Stream[Task, Event](domEvent1, domEvent2))
  val childWithDomainEvents = child.copy(domainEvents = Stream[Task, DomainEvent](domainEvent1, domainEvent2))

  it must "return parent element after append" in {
    parent.append(child).element mustEqual parent.element
  }

  it must "return binding with nested child" in {
    parent.append(child).nested must contain(child)
  }

  it must "return merged domEvents" in {
    parent.append(childWithDomEvents).domEvents.runLog.assert(_ must contain allOf(domEvent1, domEvent2))
  }

  it must "handle DOM events and return DomainEvents" in {
    parent.append(childWithDomEvents)
      .handleDomEvents(_.map {
        case `domEvent1` => domainEvent1
        case `domEvent2` => domainEvent2
        case `domEvent3` => domainEvent3
      })
      .domainEvents.runLog
      .assert(_ must contain allOf(domainEvent1, domainEvent2))
  }

  it must "handle DOM events and return no DOM Events" in {
    parent.append(childWithDomEvents)
      .handleDomEvents(_.map {
        case `domEvent1` => domainEvent1
        case `domEvent2` => domainEvent2
        case `domEvent3` => domainEvent3
      })
      .domEvents.runLog
      .assert(_ mustBe empty)
  }

  it must "return domainEvents" in {
    parent.append(childWithDomainEvents)
      .domainEvents.runLog
      .assert(_ must contain allOf(domainEvent1, domainEvent2))
  }

  it must "map dom event" in {
    childWithDomEvents
      .mapDomEvent(event => if (event == domEvent1) domEvent3 else event)
      .domEvents.runLog
      .assert(_ must (not contain domEvent1 and contain(domEvent3)))
  }

  it must "map domain event" in {
    childWithDomainEvents
      .mapDomainEvent(event => if (event == domainEvent1) domainEvent3 else event)
      .domainEvents.runLog
      .assert(_ must (not contain domainEvent1 and contain(domainEvent3)))
  }

}
