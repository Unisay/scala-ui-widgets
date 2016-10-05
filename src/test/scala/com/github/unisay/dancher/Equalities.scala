package com.github.unisay.dancher

import org.scalactic.Equality
import org.scalajs.dom
import org.scalatest.enablers.Aggregating

object Equalities {

  implicit val elementEquality: Equality[dom.Element] = new Equality[dom.Element] {
    def areEqual(a: dom.Element, b: Any): Boolean = a equals b
  }

  implicit val eventEquality: Equality[dom.Event] = new Equality[dom.Event] {
    def areEqual(a: dom.Event, b: Any): Boolean = a equals b
  }

  implicit val aggregating: Aggregating[Vector[dom.Event]] =
    Aggregating.convertEqualityToGenTraversableAggregating[dom.Event, Vector](eventEquality)

}
