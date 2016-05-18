package com.github.unisay.dancher

import org.scalajs.dom._

class Renderer {

  def render(widget: Widget): Unit =
    widget.render.foreach {
      case Log(text) ⇒
        console.log(text)
      case Alert(text) ⇒
        window.alert(text)
    }

}
