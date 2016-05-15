package com.github.unisay.dancher.bus

import utest._

object WebSocketTest extends TestSuite {
  override def tests = this {
    'init {
      WebSocket("ws://echo.websocket.org")
    }
  }
}
