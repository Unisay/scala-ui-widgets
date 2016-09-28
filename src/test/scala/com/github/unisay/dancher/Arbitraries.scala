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

  implicit val arbDomainEvent: Arbitrary[DomainEvent] = Arbitrary {
    arb[Int].map(i => new DomainEvent { override def toString: String = s"DomainEvent($i)" })
  }

  implicit val arbDomainEventRight: Arbitrary[Xor.Right[DomainEvent]] = Arbitrary {
    arb[DomainEvent].map(Xor.Right(_))
  }

  implicit val arbDomEventLeft: Arbitrary[Xor.Left[Event]] = Arbitrary {
    arb[Event].map(Xor.Left(_))
  }

  implicit val arbWidgetEvent: Arbitrary[WidgetEvent] = Arbitrary {
    Gen.oneOf(arb[Xor.Left[Event]], arb[Xor.Right[DomainEvent]])
  }

  implicit val arbWidgetEvents: Arbitrary[Flow[WidgetEvent]] = Arbitrary {
    Gen.sized(Gen.listOfN(_, arb[WidgetEvent])).map(l => Stream(l: _*))
  }

  def genBindings(n: Int): Gen[Vector[Binding]] = Gen.containerOfN[Vector, Binding](n/3, Gen.lzy(genBinding(n/3)))
  def genBinding(n: Int) = (arb[Element] |@| arb[Flow[WidgetEvent]] |@| genBindings(n/3)).map(Binding.apply)
  implicit val arbBinding: Arbitrary[Binding] = Arbitrary(Gen.sized(genBinding))

}
