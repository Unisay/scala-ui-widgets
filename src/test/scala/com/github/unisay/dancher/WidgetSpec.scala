package com.github.unisay.dancher

import com.github.unisay.dancher.Arbitraries._
import com.github.unisay.dancher.Dom.Event._
import com.github.unisay.dancher.DomArbitraries._
import com.github.unisay.dancher.Equalities._
import com.github.unisay.dancher.TestUtils._
import com.github.unisay.dancher.Widget._
import fs2.{Stream, Task}
import org.scalajs.dom._
import org.scalatest.{AsyncFlatSpec, MustMatchers}

class WidgetSpec extends AsyncFlatSpec with MustMatchers {

  implicit override def executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  val (childWidget, childBinding) = createWidget("div")
  val (parent, _) = createWidget("div")
  val domEvent1 = createDomEvent(Click)
  val domEvent2 = createDomEvent(MouseUp)
  val childWithDomEvents = childWidget.map(_.copy(domEvents = Stream[Task, Event](domEvent1, domEvent2)))

  behavior of "WidgetOps"

  it must "appendFragment" in {
    parent.appendFragment(childWithDomEvents)
      .flatMap(_.domEvents.runLog)
      .assert(_ must contain allOf(domEvent1, domEvent2))
  }

  it must "emitDomEvents" in {
    asynchronously {
      childBinding.element.sendEvent(MouseUp)
      childBinding.element.sendEvent(MouseDown)
      childBinding.element.sendEvent(Click)
    }

    childWidget
      .emitDomEvents(MouseUp, Click)
      .flatMap(_.domEvents.take(2).runLog)
      .assert(_.map(_.`type`) must contain theSameElementsInOrderAs List(MouseUp.name, Click.name))
  }

}
