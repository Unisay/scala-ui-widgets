package com.github.unisay.dancher

import cats.syntax.cartesian._
import com.github.unisay.dancher.DomArbitraries._
import com.github.unisay.dancher.GenMonad._
import fs2._
import org.scalacheck.Arbitrary.{arbitrary => arb}
import org.scalacheck.{Arbitrary, Gen}
import org.scalajs.dom.{Element, Event}

object Arbitraries {

  def sample[T: Arbitrary]: T = Arbitrary.arbitrary[T].sample.get

  def createBinding(i: Int): Binding = Binding(createElement("div"), Stream.empty, Stream.empty)

  def createWidget(i: Int): (Widget, Binding) = {
    val binding: Binding = createBinding(i)
    (Task.delay(binding), binding)
  }

  def createDomainEvent(i: Int): DomainEvent = new DomainEvent { override def toString: String = s"DomainEvent($i)" }

  implicit val arbDomainEvent: Arbitrary[DomainEvent] = Arbitrary(arb[Int] map createDomainEvent)

  implicit val arbDomainEvents: Arbitrary[Flow[DomainEvent]] = Arbitrary {
    Gen.sized(Gen.listOfN(_, arb[DomainEvent])).map(l => Stream(l: _*))
  }

  def genBindings(n: Int): Gen[Vector[Binding]] = Gen.containerOfN[Vector, Binding](n/3, Gen.lzy(genBinding(n/3)))
  def genBinding(n: Int) = (arb[Element] |@| arb[Flow[Event]] |@| arb[Flow[DomainEvent]] |@| genBindings(n/3))
    .map(Binding.apply)
  implicit val arbBinding: Arbitrary[Binding] = Arbitrary(Gen.sized(genBinding))
  implicit val arbWidget: Arbitrary[Widget] = Arbitrary(Arbitrary.arbitrary[Binding].map(Task.now))
}
