package com.github.unisay.dancher.bus

import scala.scalajs.js.JSApp

object Bus extends JSApp {

  def main(): Unit = {
    WebSocket("ws://echo.websocket.org")
  }

}
