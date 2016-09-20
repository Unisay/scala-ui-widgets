package com.github.unisay.dancher

import com.github.unisay.dancher.Dom.Event
import com.github.unisay.dancher.Dom.Event.Click
import com.github.unisay.dancher.DomArbitraries._
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{MustMatchers, PropSpec}

class DomEventSpec extends PropSpec with GeneratorDrivenPropertyChecks with MustMatchers {

  property("Event.Type.name is not empty") {
    forAll { (_: Event.Type).name must not be empty }
  }

  property("Event.Type.name contains lowercase characters only") {
    forAll { (_: Event.Type).name.forall(_.isLower) mustBe true }
  }

  property("Event.Type.name contains letter characters only") {
    forAll { (_: Event.Type).name.forall(_.isLetter) mustBe true }
  }

  property("Event.Type.name contains exact number of characters") {
    forAll(Gen.const(Click)) { _.name.length mustEqual 5 }
  }

}
