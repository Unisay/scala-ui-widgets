package com.github.unisay.dancher

import org.scalajs.dom._

class Renderer {

  def render(widget: Widget): Unit =
    widget.act.foreach {
      case Log(text, a) ⇒
        console.log(text)
      case Alert(text, a) ⇒
        window.alert(text)
      case GetElementById(id, a) ⇒

      case AppendChild(parent, child, a) ⇒

      case CreateElement(tagName, a) ⇒
        document.createElement(tagName)
    }

}
