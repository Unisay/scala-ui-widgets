package com.github.unisay.dancher

import cats.data.Ior
import com.github.unisay.dancher.ActionTestHelpers._
import com.github.unisay.dancher.ObservableMatchers._
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.interpreter.JsInterpreter
import com.github.unisay.dancher.interpreter.JsInterpreter.JsInterpreterElement
import com.github.unisay.dancher.widget.{EffectAction, RenderAction}
import com.github.unisay.dancher.widget.RenderAction._
import monix.execution.schedulers.TestScheduler
import monix.reactive.Observable
import org.scalatest.{FlatSpec, MustMatchers}

class RenderActionSpec extends FlatSpec with MustMatchers {

  implicit val scheduler = TestScheduler()
  implicit val interpreter = JsInterpreter
  import interpreter._

  def event(index: Int): DomEvent Ior EffectAction = Ior.Left(new MouseDownEvent {
    override def toString: String = s"MouseDown($index)"
  })

  val parentElement: DomElemT = JsInterpreterElement("parent")
  val childElement0: DomElemT = JsInterpreterElement("child0")
  val childElement1: DomElemT = JsInterpreterElement("child1")
  val parentEvents: DomStream = Observable(event(1), event(2))
  val childEvents: DomStream = Observable(event(3))
  val childBinding0 = DomBinding(childElement0)
  val childBinding1 = DomBinding(childElement1, domStream = childEvents)
  val parentBinding = DomBinding(parentElement, nested = Vector(childBinding0), domStream = parentEvents)
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
