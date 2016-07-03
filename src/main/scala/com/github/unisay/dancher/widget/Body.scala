package com.github.unisay.dancher.widget

import com.github.unisay.dancher.DomBinding
import com.github.unisay.dancher.dom._

case class Body(domId: DomId, children: Vector[Widget] = Vector.empty) extends WidgetContainer {
  type T = Body
  def create = getDocumentBody.map(DomBinding(_))
  def withChildren(children: Vector[Widget]): T = copy(children = children)
  def appendChild(widget: Widget): T = copy(children = children :+ widget)
}
