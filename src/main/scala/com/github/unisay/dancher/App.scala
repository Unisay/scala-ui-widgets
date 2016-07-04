package com.github.unisay.dancher

import com.github.unisay.dancher.interpreter.DomInterpreter
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.widget.{Body, Label, VerticalLayout}
import monix.execution.Ack
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

  val interpreter = new DomInterpreter()

  val frameHandler: Frame ⇒ Future[Ack] = {
    case (model, action) ⇒
      interpreter.interpret(model, action)
      Future.successful(Ack.Continue)
  }

  @JSExport
  override def main(): Unit = {
    val (m, action) = builder.build(Body('body))

    interpreter.interpret(m, action).events.foreach { (modelEvents: ModelEvents) ⇒
      val observable: Observable[Option[Frame]] = modelEvents.map {
        case (_: AddItem, model) ⇒
          model.within('labels) {
            _.label("4")
          }

        case (_: RemItem, model) ⇒
          model.modifyOpt[VerticalLayout]('vertical)(_.removeChild('baz))

        case (_: UpdateLabel, model) ⇒
          model.modify[Label]('foo)(_.setText("Yahoo!"))

      }
      observable.subscribe(frameHandler)
    }

  }

}

