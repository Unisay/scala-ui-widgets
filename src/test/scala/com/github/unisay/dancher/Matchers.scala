package com.github.unisay.dancher

import com.github.unisay.dancher.compiler.JsCompiler.{compile ⇒ compileToScript}
import com.github.unisay.dancher.dom._
import org.specs2.matcher.Matcher
import org.specs2.matcher.MustMatchers._

object Matchers {

  def beContainedInScript(substring: String): Matcher[ActionF[_]] =
    compileToScript(_: ActionF[_]).mkString("\n") must contain(substring.stripMargin.trim)

  def beActionAsScript(script: String): Matcher[ActionF[_]] =
    compileToScript(_: ActionF[_]).mkString("\n") must_=== script.stripMargin.trim

  def expectedAction(expectedAction: ActionF[_]): Matcher[ActionF[_]] =
    compileToScript(_: ActionF[_]) must_=== compileToScript(expectedAction)

}
