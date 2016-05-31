package com.github.unisay.dancher

import java.util.UUID

import com.github.unisay.dancher.dom.DomId

trait gen {

  trait Gen[A] {
    def generate: A
  }

  object Gen {
    @inline def apply[F](implicit F: Gen[F]): Gen[F] = F

    implicit val DomIdGen: Gen[DomId] = new Gen[DomId] {
      def generate: DomId = DomId(UUID.randomUUID.toString)
    }
  }

}

