package com.github.unisay.dancher

import com.github.unisay.dancher.Widget._
import fs2.Stream

object Runtime {

  type EventHandler[M] = PartialFunction[(M, DomainEvent), (Effect, M)]

  def unsafeRun[M](initialModel: M, widget: Widget)(handleEvent: EventHandler[M]): Unit = {
    val unknownEventHandler: EventHandler[M] = { case (model, event) =>
      Effect(println(s"Unknown domain event: $event")) -> model
    }

    Stream
      .eval(widget.render)
      .evalMap(binding => binding.deepDomEvents.run.map(_ => binding))
      .flatMap(binding => binding.deepDomainEvents)
      .scan((EmptyEffect, initialModel)) { case ((_, model), event) =>
        handleEvent.applyOrElse((model, event), unknownEventHandler)
      }
      .evalMap { case (task, _) => task }
      .run.unsafeRunAsyncFuture()
    ()
  }

}
