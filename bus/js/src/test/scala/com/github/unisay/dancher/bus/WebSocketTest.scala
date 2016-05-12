package com.github.unisay.dancher.bus

import utest._

class WebSocketTest extends TestSuite {
  override def tests = this {

    'init {
      WebSocket("ws://echo.websocket.org")
    }

  }
}
