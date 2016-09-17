package com.github.unisay.dancher

object CssSyntax {

  implicit class CssSuffixes(value: Int) {
    def px: String = f"$value%dpx"
  }

}
