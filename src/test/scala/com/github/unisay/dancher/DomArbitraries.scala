package com.github.unisay.dancher

import com.github.unisay.dancher.Dom.Event._
import fs2.Stream
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import org.scalajs.dom.{Element, Event, document}
import org.scalacheck.Arbitrary.{arbitrary => arb}

object DomArbitraries {

  implicit val arbElement: Arbitrary[Element] = Arbitrary {
    Gen.oneOf("div", "span", "button", "a", "h1").map(document.createElement)
  }

  implicit val arbEvent: Arbitrary[Event] = Arbitrary {
    Gen.const(new Event())
  }

  implicit val arbDomEvents: Arbitrary[Flow[Event]] = Arbitrary {
    Gen.sized(Gen.listOfN(_, arb[Event])).map(l => Stream(l: _*))
  }

  implicit val arbEventType: Arbitrary[Dom.Event.Type] = Arbitrary[Dom.Event.Type] {
    Gen.oneOf(List(Click, MouseMove, MouseUp, MouseDown, MouseEnter, MouseLeave))
  }

}
