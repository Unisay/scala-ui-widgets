package com.github.unisay.dancher

import com.github.unisay.dancher.Dom.Event._
import org.scalacheck.{Arbitrary, Gen}
import org.scalajs.dom.{Element, Event, document}

object DomArbitraries {

  implicit val arbElement: Arbitrary[Element] = Arbitrary {
    Gen.oneOf("div", "span", "button", "a", "h1").map(document.createElement)
  }

  implicit val arbEvent: Arbitrary[Event] = Arbitrary {
    Gen.const(new Event())
  }

  implicit val arbEventType: Arbitrary[Dom.Event.Type] = Arbitrary[Dom.Event.Type] {
    Gen.oneOf(List(Click, MouseMove, MouseUp, MouseDown, MouseEnter, MouseLeave))
  }

}
