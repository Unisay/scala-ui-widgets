package com.github.unisay.dancher.widget

import com.github.unisay.dancher.{DomBinding, ModelBuilder}
import com.github.unisay.dancher.dom._

case class Paragraph(domId: DomId, text: String) extends Widget {

  def create = for {
    paragraph ← createElement("p")
    _ ← paragraph setClass "d-paragraph"
    text ← createTextNode(text)
    _ ← paragraph appendChild text
  } yield DomBinding(paragraph)

}

trait ParagraphOps {
  implicit class ModelParagraphOps(model: ModelBuilder) {
    def paragraph(domId: DomId, text: String): ModelBuilder = model.appendWidget(Paragraph(domId, text))
  }
}

object Paragraph extends ParagraphOps
