package com.github.unisay.dancher

import com.github.unisay.dancher.interpreter.DomInterpreter
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.widget.{Body, Label, VerticalLayout}
import monix.execution.{Ack, Cancelable}
import monix.reactive.Observable
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.Future
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object App extends JSApp {

  case class AddItem(event: DomEvent) extends DomainEvent
  case class RemItem(event: DomEvent) extends DomainEvent
  case class UpdateLabel(event: DomEvent) extends DomainEvent

  val builder: ModelBuilder = ModelBuilder.instance

  .vertical('labels) { _
    .button(label = "Add Item", onClick = AddItem)
    .button(label = "Remove Item", onClick = RemItem)
    .button(label = "Update Foo", onClick = UpdateLabel)
  }

  .horizontal { _
    .label('foo, "Foo")
    .vertical('vertical) { _
      .label("Bar")
      .label(id = 'baz, text = "Baz")
    }
  }

  .paragraph('t, "it works!")

  val domainEventHandler: DomainEventHandler = {

    case (_: AddItem, model) ⇒
      model.within('labels) {
        _.button(label = "Add Item", onClick = AddItem)
      }.get

    case (_: RemItem, model) ⇒
      model.modifyOpt[VerticalLayout]('vertical)(_.removeChild('baz)).get

    case (_: UpdateLabel, model) ⇒
      model.modify[Label]('foo)(_.setText("Yahoo!")).get

  }

  val interpreter = new DomInterpreter()

  def handleEvents(events: Observable[ModelEvent]): Cancelable =
    events
      .map(domainEventHandler)
      .subscribe(nextFn = { case frame ⇒
        val DomBinding(_, frameEvents) = interpreter.interpret(frame)
        frameEvents.foreach(handleEvents)
        Future.successful(Ack.Continue)
      })


  @JSExport
  override def main(): Unit = {
    val frame = builder.build(Body('body))
    val initialBinding = interpreter.interpret(frame)
    initialBinding.events.foreach(handleEvents)
  }

}

