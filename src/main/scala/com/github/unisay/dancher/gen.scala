package com.github.unisay.dancher

import java.util.UUID

import com.github.unisay.dancher.dom.DomId

trait gen {

  trait Generator[A] {
    def generate: A
  }

  object Generator {
    @inline def apply[F](implicit F: Generator[F]): Generator[F] = F

    implicit val DomIdGen: Generator[DomId] = new Generator[DomId] {
      def generate: DomId = DomId(UUID.randomUUID.toString)
    }
  }

}

