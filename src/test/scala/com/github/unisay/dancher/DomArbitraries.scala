package com.github.unisay.dancher

import com.github.unisay.dancher.Dom.Event._
import org.scalacheck.{Arbitrary, Gen}

object DomArbitraries {

  implicit val arbEventType: Arbitrary[Dom.Event.Type] = Arbitrary[Dom.Event.Type] {
    Gen.oneOf(List(Click, MouseMove, MouseUp, MouseDown, MouseEnter, MouseLeave))
  }

}
