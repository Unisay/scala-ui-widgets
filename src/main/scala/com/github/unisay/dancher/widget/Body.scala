package com.github.unisay.dancher.widget

import com.github.unisay.dancher.Widget
import com.github.unisay.dancher.dom._

case class Body(domId: DomId)

object Body {

  implicit val WidgetBody = new Widget[Body] {
    def domId(body: Body): DomId = body.domId
    def create(body: Body): ActionF[DomElement] = getDocumentBody
    def children: Traversable[Widget[_]] = Seq.empty
  }

}
