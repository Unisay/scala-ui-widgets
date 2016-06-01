package com.github.unisay.dancher

import dom._

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object App extends JSApp {

  case class AddItem(event: DomEvent) extends DomainEvent
  case class RemoveItem(event: DomEvent) extends DomainEvent
  case class UpdateFoo(event: DomEvent) extends DomainEvent

  val builder = ModelBuilder()
  .vertical('labels) { _
    .button(label = "Add Item", onClick = AddItem)
    .button(label = "Remove Item", onClick = RemoveItem)
    .button(label = "Update Foo", onClick = UpdateFoo)
  }
  .horizontal { _
    .label('foo, "Foo")
    .vertical('vertical) { _
      .label("Bar")
      .label(id = 'baz, text = "Baz")
    }
  }

  @JSExport
  override def main(): Unit = {
    Runtime(builder) {

      case (_: AddItem, model) ⇒
        model.at('labels) { _.label("4") }

      case (_: RemoveItem, model) ⇒
        model.remove('baz)

      case (_: UpdateFoo, model) ⇒
        model.modify[Label]('foo) { _.setText("Yahoo!") }

    }.run()
  }

}

