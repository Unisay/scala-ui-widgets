package com.github.unisay.dancher

import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.interpreter.JsInterpreter._
import com.github.unisay.dancher.widget.RenderAction
import monix.reactive.Observable
import org.scalajs.dom.Element

object ActionTestHelpers {

  private def interpretJs0[M, R](action: ActionF[R], model: M, domEvents: Observable[(String, DomEvent)] = Observable.empty): (R, String) =
    interpretScript[R, M](model, action, domEvents)

  implicit class ActionTestOps[R](val action: ActionF[R]) extends AnyVal {
    def interpretJs[M](model: M, domEvents: Observable[(String, DomEvent)] = Observable.empty): (R, String) =
      interpretJs0(action, model, domEvents)
    def interpretJsString[M](model: M): String = interpretScript(model, action)._2
  }

  implicit class RenderActionTestOps[M](val renderAction: RenderAction[M]) {
    def interpretJs(model: M, domEvents: Observable[(String, DomEvent)] = Observable.empty):
      (Element, Vector[DomBinding[M]], DomainStream[M], String) = {
      val (domBinding, script) = interpretJs0(renderAction, model, domEvents)
      (domBinding.element, domBinding.nested, domBinding.domainStream, script)
    }
  }

}
