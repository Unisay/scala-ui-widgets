package com.github.unisay.dancher

case class Paragraph(text: String) extends Widget {
  def create = for {
    paragraph ← createElement("p")
    text ← createTextNode(text)
    _ ← appendChild(paragraph, text)
  } yield paragraph
}

case class Button(label: String) extends Widget {
  def create = for {
    button ← createElement("button")
    label ← createTextNode(label)
    _ ← appendChild(button, label)
  } yield button
}
