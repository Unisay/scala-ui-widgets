package com.github.unisay.dancher

import com.github.unisay.dancher.DemoPage.Model
import com.github.unisay.dancher.widget.all._
import com.github.unisay.dancher.interpreter.DomInterpreter
import monix.execution.Scheduler.Implicits.global

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object App extends JSApp {

  val demoModel = Model(activeTab = 1)

  val interpreter = new DomInterpreter()

  @JSExport
  override def main(): Unit = {
    println("App started")

    val binding = interpreter.interpret(demoModel, DemoPage(Body).render(demoModel))
    binding.events.foreach(println)

    /*
      .subscribe(or =>
      or match {
        case Ior.Right(TabActivated(index)) =>
          println(s"Tab $index activated!")
          Ack.Continue
        case Ior.Left(model) =>
          println(s"Model updated: $model")
          Ack.Continue
        case _ =>
          println("Something happened!")
          Ack.Continue
      }
    )
*/
    ()
  }

}

