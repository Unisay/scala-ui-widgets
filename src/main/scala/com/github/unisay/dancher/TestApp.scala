package com.github.unisay.dancher

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom._

object TestApp extends JSApp {

  @JSExport
  override def main(): Unit = {
    println("TestApp started")

    val el = document.createElement("div")
    el.appendChild(document.createTextNode("CLICKME"))
    el.addEventListener("click", (evt: Event) => {
      console.info("is same node: " + el.isSameNode(evt.target.asInstanceOf[Node]))
      console.dir(evt.target)
    })
    document.body.appendChild(el)
    ()
  }

}
