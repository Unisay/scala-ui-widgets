package com.github.unisay.dancher

import com.github.unisay.dancher.compiler.JsCompiler
import com.github.unisay.dancher.dom._
import org.specs2.matcher.Matcher
import org.specs2.matcher.MustMatchers._

object Matchers {

  def beActionAsScript(script: String): Matcher[ActionF[_]] =
    new JsCompiler().compile(_: ActionF[_]).mkString("\n") must_=== script.stripMargin.trim

  def expectedAction(expectedAction: ActionF[_]): Matcher[ActionF[_]] =
    new JsCompiler().compile(_: ActionF[_]) must_=== new JsCompiler().compile(expectedAction)

}
