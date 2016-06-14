package com.github.unisay.dancher.widget

import com.github.unisay.dancher.Widget
import com.github.unisay.dancher.dom._

case class Paragraph(domId: DomId, text: String)

object Paragraph {

  implicit val WidgetParagraph = new Widget[Paragraph] {

    def domId(paragraph: Paragraph): DomId = paragraph.domId

    def create(paragraph: Paragraph): ActionF[DomElement] =
      for {
        p ← createElement("p")
        _ ← p setClass "d-paragraph"
        text ← createTextNode(paragraph.text)
        _ ← p appendChild text
      } yield p
  }

}

