package com.github.unisay.dancher.bus

import org.scalajs.dom

case class WebSocket(url: String) {

  val socket = new dom.raw.WebSocket(url)

  socket.onmessage = {
    (e: dom.MessageEvent) ⇒
      println(e.data)
  }

  socket.onopen = { (e: dom.Event) ⇒
    socket.send("test")
  }

}
