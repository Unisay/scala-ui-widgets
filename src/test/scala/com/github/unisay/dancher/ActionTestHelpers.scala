package com.github.unisay.dancher

import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.interpreter.JsInterpreter._
import com.github.unisay.dancher.widget.RenderAction
import monix.reactive.Observable

object ActionTestHelpers {

  private def interpretJs0[M, R](action: ActionF[R],
                                 model: M,
                                 domEvents: Observable[(String, DomEvent)] = Observable.empty,
                                 attributes: Map[JsInterpreterElement, Map[String, String]] = Map.empty): (R, String) =
    interpretScript[R, M](model, action, domEvents, attributes)

  implicit class ActionTestOps[R](val action: ActionF[R]) extends AnyVal {
    def interpretJs[M](model: M,
                       domEvents: Observable[(String, DomEvent)] = Observable.empty,
                       attributes: Map[JsInterpreterElement, Map[String, String]] = Map.empty): (R, String) =
      interpretJs0(action, model, domEvents, attributes)
    def interpretJsString[M](model: M): String = interpretScript(model, action)._2
  }

  implicit class RenderActionTestOps[E: DomElem, M](val renderAction: RenderAction[E, M]) {
    def interpretJs(model: M,
                       domEvents: Observable[(String, DomEvent)] = Observable.empty,
                       attributes: Map[JsInterpreterElement, Map[String, String]] = Map.empty):
      (E, Vector[DomBinding[E, M]], DomainStream[M], String) = {
      val (domBinding, script) = interpretJs0(renderAction, model, domEvents, attributes)
      (domBinding.element, domBinding.nested, domBinding.domainStream, script)
    }
  }

}
