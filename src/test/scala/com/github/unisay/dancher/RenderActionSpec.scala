package com.github.unisay.dancher

/*

class RenderActionSpec extends FlatSpec with MustMatchers {

  implicit val scheduler = TestScheduler()
  val interpreter = JsInterpreter
  import interpreter._

  def event(index: Int): DomEvent Ior EffectAction = Ior.Left(arbitraryDomEvent.arbitrary.sample.get)

  val parentElement: Element = JsInterpreterElement("parent")
  val childElement0: DomElemT = JsInterpreterElement("child0")
  val childElement1: DomElemT = JsInterpreterElement("child1")
  val parentEvents: DomStream = Observable(event(1), event(2))
  val childEvents: DomStream = Observable(event(3))
  val childBinding0 = DomBinding[DomElemT, Unit](childElement0)
  val childBinding1 = DomBinding[DomElemT, Unit](childElement1, domStream = childEvents)
  val parentBinding = DomBinding[DomElemT, Unit](parentElement, nested = Vector(childBinding0), domStream = parentEvents)
  val parentAction: RenderAction[DomElemT, Unit] = dom.value(parentBinding)
  val childAction: RenderAction[DomElemT, Unit] = dom.value(childBinding1)

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
*/
