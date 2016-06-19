package com.github.unisay.dancher

import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.widget._

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

  .paragraph('t, "it works!")

  @JSExport
  override def main(): Unit = {
    Runtime(bodyModel) {

      case (_: AddItem, model) ⇒
        model.within('labels) { _.label("4") }

      case (_: RemItem, model) ⇒
        model.modify[VerticalLayout]('vertical)(_.removeChild('baz))

      case (_: UpdateLabel, model) ⇒
        model.modify[Label]('foo)(_.setText("Yahoo!"))

    }.run()
  }

}

