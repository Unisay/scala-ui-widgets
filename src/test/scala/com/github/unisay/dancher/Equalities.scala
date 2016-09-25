package com.github.unisay.dancher

import org.scalactic.Equality
import org.scalajs.dom
import org.scalajs.dom._

object Equalities {

  implicit val elementEquality: Equality[dom.Element] = new Equality[Element] {
    def areEqual(a: Element, b: Any): Boolean = a.equals(b)
  }

}
