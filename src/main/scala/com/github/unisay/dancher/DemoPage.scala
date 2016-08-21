package com.github.unisay.dancher

import com.github.unisay.dancher.dom.MouseEnter
import com.github.unisay.dancher.interpreter.ActionInterpreter
import com.github.unisay.dancher.widget.Widget
import com.github.unisay.dancher.widget.all._
import monocle.macros.Lenses

object DemoPage {

  @Lenses case class Model(tabs: TabsModel)

  def apply(parent: Widget[Model])(implicit ai: ActionInterpreter): Widget[Model] = {
    import ai._

    parent +>
      Vertical {
        Header("Demo Page") >
          Tabs(Model.tabs, switchOn = MouseEnter)(
            "Impression" -> Header("First Tab",  size = 2),
            "Contacts"   -> Header("Second Tab", size = 2),
            "About Us"   -> Header("Third Tab",  size = 2)
          )
      }
  }

}
