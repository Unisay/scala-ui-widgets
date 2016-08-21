package com.github.unisay.dancher

import com.github.unisay.dancher.DemoPage.Model
import com.github.unisay.dancher.widget.all._
import com.github.unisay.dancher.interpreter.{DomInterpreter, JsInterpreter}
import com.github.unisay.dancher.widget.EffectAction
import monix.execution.Ack
import monix.execution.Scheduler.Implicits.global

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object App extends JSApp {

  val demoModel = Model(tabs = TabsModel(activeTab = 1))

  implicit val interpreter = new DomInterpreter()
  import interpreter._

  @JSExport
  def debug(action: EffectAction): String = JsInterpreter.interpretScript(demoModel, action)._2

  @JSExport
  override def main(): Unit = {
    println("App started")

    DemoPage(Body).render(demoModel).events.subscribe(nextFn = {
      case (_, TabActivated(index)) =>
        println(s"Tab $index activated!")
        Ack.Continue
    })

    ()
  }

}

