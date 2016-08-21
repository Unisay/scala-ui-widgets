package com.github.unisay.dancher

import cats.data.Ior
import com.github.unisay.dancher.ActionMatchers._
import com.github.unisay.dancher.ObservableMatchers._
import com.github.unisay.dancher.RenderAction._
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.interpreter.JsInterpreter.RawElement
import monix.reactive.Observable
import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable.Specification

class RenderActionSpec(implicit ee: ExecutionEnv) extends Specification {

  case class Ev(value: Int) extends DomainEvent
  def event(value: Int): Unit Ior DomainEvent = Ior.right(Ev(value))

  val parentElement: DomElement = RawElement("parent")
  val childElement: DomElement = RawElement("child")
  val parentEvents = Observable(event(1), event(2))
  val childEvents = Observable(event(3))
  val parentAction: RenderAction[Unit] = dom.value(DomBinding(element = parentElement, events = parentEvents))
  val childAction: RenderAction[Unit] = dom.value(DomBinding(element = childElement, events = childEvents))

  "RenderAction" should {

    "appendReturningParent" in {
      val renderAction = appendReturningParent(parentAction, childAction)

      renderAction.asScriptString must beEqualTo("parent.appendChild(child);")
      val DomBinding(element, events) = renderAction.asValue
      element must beEqualTo(parentElement)
      events.toList must beEqualTo(List(event(1), event(2), event(3)))
    }

    "appendReturningChild" in {
      val renderAction = appendReturningChild(parentAction, childAction)

      renderAction.asScriptString must beEqualTo("parent.appendChild(child);")
      val DomBinding(element, events) = renderAction.asValue
      element must beEqualTo(childElement)
      events.toList must beEqualTo(List(event(1), event(2), event(3)))
    }

  }
}
