package com.github.unisay.dancher

import fs2.{Pipe, Task}

object StreamUtils {

  def logIt[I](prefix: String): Pipe[Task, I, I] = _.evalMap { item =>
    Task.delay {
      println(prefix + "> " + item)
      item
    }
  }

}
