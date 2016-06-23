package com.github.unisay.dancher

import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.widget.VerticalLayout._
import com.github.unisay.dancher.widget.HorizontalLayout._
import com.github.unisay.dancher.widget.Label._
import com.github.unisay.dancher.widget.Button._
import com.github.unisay.dancher.widget.Paragraph._

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object App extends JSApp {

  case class AddItem(event: DomEvent) extends DomainEvent
  case class RemItem(event: DomEvent) extends DomainEvent
  case class UpdateLabel(event: DomEvent) extends DomainEvent

  val builder = ModelBuilder.instance

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

  @JSExport
  override def main(): Unit = {
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

