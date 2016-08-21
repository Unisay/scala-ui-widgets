package com.github.unisay.dancher

import cats.data.Ior
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.interpreter.JsInterpreter._
import com.github.unisay.dancher.widget.RenderAction
import monix.reactive.Observable
import org.specs2.matcher.MustMatchers._
import org.specs2.matcher.{ContainWithResultSeq, Matcher, ValueCheck}

object ActionMatchers {

  implicit class ActionAs[M](actionF: RenderAction[M]) {
    def interpretJs(model: M, domEvents: Observable[(String, DomEvent)] = Observable.empty):
    (DomElement, Observable[Ior[M, DomainEvent]], String) = {
      val result = interpret(model, actionF, domEvents)
      (result._1.element, result._1.events, result._2.mkString("", ";\n", ";").trim)
    }
    def interpretJs: (DomElement, Observable[Ior[M, DomainEvent]], String) = interpretJs(model = ().asInstanceOf[M])

    def interpretJsString(model: M): String = interpretJs(model)._3
    def interpretJsString: String = interpretJsString(model = ().asInstanceOf[M])
  }

  def containScriptTemplate(lines: String): Matcher[Script] = {
    val checks: Seq[ValueCheck[String]] = lines.split("\n").toSeq.map(_.trim).filterNot(_.isEmpty).map(beMatching(_))
    (_: Script) must ContainWithResultSeq(checks)
  }

  def expectedAction[A](expectedAction: ActionF[A]): Matcher[ActionF[A]] =
    interpret((), _: ActionF[A]) must_=== interpret((), expectedAction)

}
