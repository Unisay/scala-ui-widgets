package com.github.unisay.dancher

import cats.data.Ior
import com.github.unisay.dancher.ActionTestHelpers._
import com.github.unisay.dancher.ObservableMatchers._
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.interpreter.JsInterpreter.RawElement
import com.github.unisay.dancher.widget.RenderAction
import com.github.unisay.dancher.widget.all._
import monix.execution.schedulers.TestScheduler
import monix.reactive.Observable
import org.scalatest.{FlatSpec, MustMatchers}

class RenderActionSpec extends FlatSpec with MustMatchers {

  implicit val scheduler = TestScheduler()

  case class Ev(value: Int) extends DomainEvent
  def event(value: Int): Unit Ior DomainEvent = Ior.right(Ev(value))

  val parentElement: DomElement = RawElement("parent")
  val childElement: DomElement = RawElement("child")
  val parentEvents = Observable(event(1), event(2))
  val childEvents = Observable(event(3))
  val parentAction: RenderAction[Unit] = dom.value(DomBinding(element = parentElement, events = parentEvents))
  val childAction: RenderAction[Unit] = dom.value(DomBinding(element = childElement, events = childEvents))

  behavior of "RenderAction"

  it must "appendReturningParent" in {
    val renderAction = appendReturningParent(parentAction, childAction)

    renderAction.interpretJsString(()) mustEqual "parent.appendChild(child);"
    val (element, events, _) = renderAction.interpretJs
    element mustBe parentElement
    events.toList() must contain theSameElementsInOrderAs List(event(1), event(2), event(3))
  }

  it must "appendReturningChild" in {
    val renderAction = appendReturningChild(parentAction, childAction)

    renderAction.interpretJsString(()) mustEqual "parent.appendChild(child);"
    val (element, events, _) = renderAction.interpretJs
    element mustBe childElement
    events.toList() must contain theSameElementsInOrderAs List(event(1), event(2), event(3))
  }

}
