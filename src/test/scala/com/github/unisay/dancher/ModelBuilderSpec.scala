package com.github.unisay.dancher

import org.specs2.mutable.Specification

class ModelBuilderSpec extends Specification {

  "ModelBuilder must" >> {
    ModelBuilder().vertical { _.label('a, "test") }
    ok
  }

}
