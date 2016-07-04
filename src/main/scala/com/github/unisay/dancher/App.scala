package com.github.unisay.dancher

import com.github.unisay.dancher.compiler.DomInterpreter
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.widget.Body
import monix.execution.Ack
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
    .button(label = "Add Item")
    .button(label = "Remove Item")
    .button(label = "Update Foo")
  }

  .horizontal { _
    .label('foo, "Foo")
    .vertical('vertical) { _
      .label("Bar")
      .label(id = 'baz, text = "Baz")
    }
  }

  .paragraph('t, "it works!")

  val compiler = new DomInterpreter()

  @JSExport
  override def main(): Unit = {
    val (model, action) = builder.build(Body('body))
    val domBinding: DomBinding = compiler.interpret(model, action)
    domBinding.events.foreach { modelEvents ⇒
      modelEvents.subscribe((modelEvent: ModelEvent) ⇒ modelEvent match {
        case (event, m) ⇒
          println(event)
          Future.successful(Ack.Continue)
      })
    }

/*
    Runtime(bodyModel) {

      case (_: AddItem, model) ⇒
        model.within('labels) { _.label("4") }

      case (_: RemItem, model) ⇒
        model.modifyOpt[VerticalLayout]('vertical)(_.removeChild('baz))

      case (_: UpdateLabel, model) ⇒
        model.modify[Label]('foo)(_.setText("Yahoo!"))

    }.run()
*/
  }

}

