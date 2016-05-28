package com.github.unisay.dancher

import monocle.macros.{GenLens, GenPrism}

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object App extends JSApp {

  case class AddItem(event: DomEvent) extends DomainEvent
  case class RemoveItem(event: DomEvent) extends DomainEvent

  lazy val layout = VerticalLayout(
    HorizontalLayout(
      Button(label = "Add Item", onClick = Some(AddItem)) ::
      Button(label = "Remove Item", onClick = Some(RemoveItem)) ::
      Nil
    ) ::
    VerticalLayout(
      Label("1") ::
      Label("2") ::
      Label("3") ::
      Nil
    ) :: Nil
  )

  val defaultComparator = new DefaultModelComparator

  @JSExport
  override def main(): Unit = {
    Runtime(defaultComparator, layout) {
      case (_: AddItem, model) ⇒
        GenPrism[Model, VerticalLayout]
          .composeLens(GenLens[VerticalLayout](_.models))
          .modify(_ :+ Label("4"))(model)

      case (_: RemoveItem, model) ⇒
        GenPrism[Model, VerticalLayout]
          .composeLens(GenLens[VerticalLayout](_.models))
          .modify(_.dropRight(1))(model)
    }.run()
  }

}

