package com.github.unisay.dancher

import cats.data.Ior
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.interpreter.JsInterpreter._
import com.github.unisay.dancher.widget.RenderAction
import monix.reactive.Observable

object ActionTestHelpers {

  implicit class ActionAs[M](actionF: RenderAction[M]) {
    def interpretJs(model: M, domEvents: Observable[(String, DomEvent)] = Observable.empty):
    (DomElement, Observable[Ior[M, DomainEvent]], String) = {
      val result = interpret(model, actionF, domEvents)
      (result._1.element, result._1.events, result._2.mkString("", ";\n", ";").trim)
    }
    def interpretJs: (DomElement, Observable[Ior[M, DomainEvent]], String) = interpretJs(model = ().asInstanceOf[M])

    def interpretJsString(model: M): String = interpretJs(model)._3
  }

}
