package com.github.unisay.dancher

import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.interpreter.JsInterpreter._
import monix.reactive.Observable
import org.specs2.matcher.MustMatchers._
import org.specs2.matcher.{ContainWithResultSeq, Matcher, ValueCheck}

object ActionMatchers {

  implicit class ActionAs[A](actionF: ActionF[A]) {
    def asValue[M](model: M, domEvents: Observable[(String, DomEvent)] = Observable.empty): A =
      interpret(model, actionF, domEvents)._1
    def asValue: A = asValue(())
    def asScript[M](model: M): Script = interpret((), actionF)._2
    def asScript: Script = asScript(())
    def asScriptString[M](model: M): String = asScript(model).mkString("", ";\n", ";").trim
    def asScriptString: String = asScriptString(())
  }

  def containScriptTemplate(lines: String): Matcher[Script] = {
    val checks: Seq[ValueCheck[String]] = lines.split("\n").toSeq.map(_.trim).filterNot(_.isEmpty).map(beMatching(_))
    (_: Script) must ContainWithResultSeq(checks)
  }

  def expectedAction[A](expectedAction: ActionF[A]): Matcher[ActionF[A]] =
    interpret((), _: ActionF[A]) must_=== interpret((), expectedAction)

}
