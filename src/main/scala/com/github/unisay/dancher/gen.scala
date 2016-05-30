package com.github.unisay.dancher

import java.util.UUID

import com.github.unisay.dancher.dom.DomId

trait gen {

  trait Gen[A] {
    def generate: A
  }

  object Gen {
    implicit val DomIdGen: Gen[DomId] = new Gen[DomId] {
      def generate: DomId = DomId(UUID.randomUUID.toString)
    }
  }

}

