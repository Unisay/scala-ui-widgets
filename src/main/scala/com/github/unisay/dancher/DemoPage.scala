package com.github.unisay.dancher

import com.github.unisay.dancher.widget.Widget
import com.github.unisay.dancher.widget.all._
import monocle.macros.Lenses

object DemoPage {

  @Lenses case class Model(activeTab: Int)

  def apply(parent: Widget[Model]): Widget[Model] =
    parent +>
      Vertical {
        Header("Demo Page") >
          Tabs(Model.activeTab)(
            "First"  -> Header("First Tab",  size = 2),
            "Second" -> Header("Second Tab", size = 2),
            "Third"  -> Header("Third Tab",  size = 2)
          )
      }

}
