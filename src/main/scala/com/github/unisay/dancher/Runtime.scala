package com.github.unisay.dancher

import com.outr.scribe.Logging
import fs2.Stream

object Runtime extends Logging {

  type EventHandler[M] = PartialFunction[(M, DomainEvent), (Effect, M)]

  def unsafeRun[M](initialModel: M, widget: Widget)(handleEvent: EventHandler[M]): Unit = {
    val unknownEventHandler: EventHandler[M] = { case (model, event) =>
      Effect(logger.error(s"Unknown domain event: $event")) -> model
    }

    Stream
      .eval(widget flatMap (_.render))
      .flatMap(_.deepEvents)
      .filter(_.isRight)
      .map(_.toOption.get)
      .scan((EmptyEffect, initialModel)) { case ((_, model), event) =>
        handleEvent.applyOrElse((model, event), unknownEventHandler)
      }
      .evalMap { case (task, _) => task }
      .run.unsafeRunAsyncFuture()
    ()
  }

}
