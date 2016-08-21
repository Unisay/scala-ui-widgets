package com.github.unisay.dancher

import com.github.unisay.dancher.widget.all._
import com.github.unisay.dancher.interpreter.DomInterpreter
import monix.execution.Cancelable
import monix.execution.Scheduler.Implicits.global
import monix.reactive.{Observable, OverflowStrategy}
import org.scalajs.dom.document
import org.scalajs.dom.raw.MouseEvent

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object TestApp extends JSApp {

  val interpreter = new DomInterpreter()

  @JSExport
  override def main(): Unit = {
    println("TestApp started")

    val widget = Body[Unit] +> Button[Unit](const("Click it"))

    val binding = interpreter.interpret((), widget.render(()))

    binding.events.foreach { i =>
      println(i)
      ()
    }

    val button = document.createElement("button")
    button.appendChild(document.createTextNode("Click Me!"))
    document.body.appendChild(button)

    val observable = Observable.create[Int](OverflowStrategy.Fail(10)) { subscriber =>
      val listener = (mouseEvent: MouseEvent) => {
        Observable(1, 2, 3, 4, 5).foreach { item =>
          subscriber.onNext(item)
          ()
        }
      }

      button.addEventListener("click", listener)
      Cancelable(() => button.removeEventListener("click", listener))
    }

    observable.foreach(println)

    ()
  }

}
