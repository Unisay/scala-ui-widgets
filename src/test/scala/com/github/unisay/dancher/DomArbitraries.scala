package com.github.unisay.dancher

import com.github.unisay.dancher.Dom.Event._
import fs2.Stream
import org.scalacheck.Arbitrary.{arbitrary => arb}
import org.scalacheck.{Arbitrary, Gen}
import org.scalajs.dom.{Element, Event, document}

object DomArbitraries {

  def createDomEvent(i: Int): Event = new Event() { override def toString: String = s"Event#$i" }
  def createElement(tag: String): Element = document.createElement(tag)

  implicit val arbElement: Arbitrary[Element] = Arbitrary(Gen.oneOf("div", "span", "a", "h1") map createElement)

  implicit val arbEvent: Arbitrary[Event] = Arbitrary(Gen.posNum[Int] map createDomEvent)

  implicit val arbDomEvents: Arbitrary[Flow[Event]] = Arbitrary {
    Gen.sized(Gen.listOfN(_, arb[Event])).map(l => Stream(l: _*))
  }

  implicit val arbEventType: Arbitrary[Dom.Event.Type] = Arbitrary[Dom.Event.Type] {
    Gen.oneOf(List(Click, MouseMove, MouseUp, MouseDown, MouseEnter, MouseLeave))
  }

}
