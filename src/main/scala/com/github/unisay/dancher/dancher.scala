package com.github.unisay.dancher

import dom._

trait ModelComparator {
  def diff(old: Model, updated: Model): ActionF[_]
}

trait DomainEvent