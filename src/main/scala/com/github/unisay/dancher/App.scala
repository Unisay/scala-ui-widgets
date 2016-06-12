package com.github.unisay.dancher

import dom._

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object App extends JSApp {

  case class AddItem(event: DomEvent) extends DomainEvent
  case class RemItem(event: DomEvent) extends DomainEvent
  case class UpdateLabel(event: DomEvent) extends DomainEvent

  val bodyModel = Model.body
  .vertical('labels) { _
    .button(label = "Add Item",    onClick = AddItem)
    .button(label = "Remove Item", onClick = RemItem)
    .button(label = "Update Foo",  onClick = UpdateLabel)
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
    Runtime(bodyModel) {

      case (_: AddItem, model) ⇒
        model.within('labels) { _.label("4") }

      case (_: RemItem, model) ⇒
        model.remove('baz)

      case (_: UpdateLabel, model) ⇒
        model.modify[Label]('foo) { _.setText("Yahoo!") }

    }.run()
  }

}

