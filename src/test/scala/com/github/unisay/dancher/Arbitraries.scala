package com.github.unisay.dancher

import cats.data.Xor
import cats.syntax.cartesian._
import com.github.unisay.dancher.DomArbitraries._
import com.github.unisay.dancher.GenMonad._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.{arbitrary => arb}
import org.scalajs.dom.{Element, Event}
import fs2._

object Arbitraries {

  def sample[T: Arbitrary]: T = Arbitrary.arbitrary[T].sample.get

  implicit val arbDomainEvent: Arbitrary[DomainEvent] = Arbitrary {
    arb[Int].map(i => new DomainEvent { override def toString: String = s"DomainEvent($i)" })
  }

  implicit val arbDomainEvents: Arbitrary[Flow[DomainEvent]] = Arbitrary {
    Gen.sized(Gen.listOfN(_, arb[DomainEvent])).map(l => Stream(l: _*))
  }

  def genBindings(n: Int): Gen[Vector[Binding]] = Gen.containerOfN[Vector, Binding](n/3, Gen.lzy(genBinding(n/3)))
  def genBinding(n: Int) = (arb[Element] |@| arb[Flow[Event]] |@| arb[Flow[DomainEvent]] |@| genBindings(n/3))
    .map(Binding.apply)
  implicit val arbBinding: Arbitrary[Binding] = Arbitrary(Gen.sized(genBinding))
  implicit val arbWidget: Arbitrary[Widget] = Arbitrary(Arbitrary.arbitrary[Binding].map(Task.now))
}
