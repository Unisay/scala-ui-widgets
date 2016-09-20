package com.github.unisay.dancher

object Syntax {

  implicit class PartialToTotal[T](val pf: PartialFunction[T, T]) extends AnyVal {
    def total: T => T = t => pf.applyOrElse(t, identity[T])
  }

}
