package com.github.unisay.dancher

import cats.data.NonEmptyList
import org.scalacheck.{Arbitrary, Gen}

object Arbitraries {

  implicit def arbitraryNonEmptyList[T](implicit T: Arbitrary[T]): Arbitrary[NonEmptyList[T]] = Arbitrary {
    Gen.nonEmptyListOf(T.arbitrary).map(NonEmptyList.fromListUnsafe(_))
  }

}
