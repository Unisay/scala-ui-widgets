package com.github.unisay.dancher

import com.github.unisay.dancher.dom.{DomElem, MouseEnter}
import com.github.unisay.dancher.widget.Widget
import com.github.unisay.dancher.widget.Widget._
import monocle.macros.Lenses

object DemoPage {

  @Lenses case class Model(tabs: TabsModel, text1: String, text2: String, text3: String)

  def apply[E: DomElem](parent: Widget[E, Model]): Widget[E, Model] = {

    val h = Horizontal {
      Vertical {
        Text(Model.text1) >
        Text(Model.text2) >
        Text(Model.text3)
      } >
        Vertical {
          Header("Demo Page") >
            Tabs(Model.tabs, switchOn = MouseEnter)(
              "Impression" -> Header("First Tab",  size = 2),
              "Contacts"   -> Header("Second Tab", size = 2),
              "About Us"   -> Header("Third Tab",  size = 2)
            )
        }
    }

    parent +> HorizontalSplit(
      Text(Model.text1),
      Vertical(
        Text(const("Row 1")) >
        Text(const("Row 2")) >
        Text(const("Row 3"))
      )
    )
  }

}
