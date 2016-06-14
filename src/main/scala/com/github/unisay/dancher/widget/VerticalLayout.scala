package com.github.unisay.dancher.widget

import com.github.unisay.dancher.Widget
import com.github.unisay.dancher.dom._


object VerticalLayout {

  implicit def WidgetVerticalLayout[W : Widget] = new Widget[VerticalLayout[W]] {

    def domId(w: VerticalLayout[W]): DomId = w.domId

    def create(w: VerticalLayout[W])(implicit ev: HasChildren[w.type]): ActionF[DomElement] = {
      for {
        div ← createElement("div")
        _ ← div setId w.domId
        children ← w.createChildren
        elements ← children.toList.map(_.create).sequence
        _ ← elements.map(div.appendChild).sequence
      } yield div
    }
  }

}

case class VerticalLayout[W : Widget](domId: DomId, children: Vector[W])
