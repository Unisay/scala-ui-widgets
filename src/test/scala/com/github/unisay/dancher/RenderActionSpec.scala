package com.github.unisay.dancher

import com.github.unisay.dancher.ActionTestHelpers._
import com.github.unisay.dancher.ObservableMatchers._
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.interpreter.JsInterpreter
import com.github.unisay.dancher.interpreter.JsInterpreter.JsInterpreterElement
import com.github.unisay.dancher.widget.RenderAction
import com.github.unisay.dancher.widget.RenderAction._
import monix.execution.schedulers.TestScheduler
import monix.reactive.Observable
import org.scalatest.{FlatSpec, MustMatchers}

class RenderActionSpec extends FlatSpec with MustMatchers {

  implicit val scheduler = TestScheduler()
  implicit val interpreter = JsInterpreter
  import interpreter._

  case class TestDomainEvent(index: Int) extends DomainEvent

  case class Ev(value: Int) extends DomainEvent
  def event(index: Int): (Unit, DomainEvent) = ((), TestDomainEvent(index))

  val parentElement: DomElemT = JsInterpreterElement("parent")
  val childElement0: DomElemT = JsInterpreterElement("child0")
  val childElement1: DomElemT = JsInterpreterElement("child1")
  val parentEvents = Observable(event(1), event(2))
  val childEvents = Observable(event(3))
  val childBinding0 = DomBinding(childElement0)
  val childBinding1 = DomBinding(childElement1, events0 = childEvents)
  val parentBinding = DomBinding(parentElement, Vector(childBinding0), parentEvents)
  val parentAction: RenderAction = dom.value(parentBinding)
  val childAction: RenderAction = dom.value(childBinding1)

  behavior of "RenderAction"

  it must "append" in {
    val renderAction = append(parentAction, childAction)

    val (element, nested, events, script) = renderAction.interpretJs(model = ())
    element mustBe parentElement
    nested must contain theSameElementsInOrderAs List(childBinding0, childBinding1)
    script mustEqual "parent.appendChild(child1);"
    events.toList() must contain theSameElementsInOrderAs List(event(1), event(2), event(3))
  }

}
