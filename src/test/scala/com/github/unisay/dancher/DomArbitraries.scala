package com.github.unisay.dancher

import com.github.unisay.dancher.dom.DomEvent
import org.scalacheck.{Arbitrary, Gen}

object DomArbitraries {

  implicit val arbDomEvent = Arbitrary {
    Gen.numChar.map(index => new DomEvent() { override def toString = s"domEvent$index" })
  }

}
