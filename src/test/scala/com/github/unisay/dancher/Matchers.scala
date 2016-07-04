package com.github.unisay.dancher

import com.github.unisay.dancher.interpreter.JsInterpreter.{Script, compile ⇒ compileToScript}
import com.github.unisay.dancher.dom._
import org.specs2.matcher.MustMatchers._
import org.specs2.matcher.{ContainWithResultSeq, Matcher, ValueCheck}

object Matchers {

  implicit class ActionAsScript(actionF: ActionF[_]) {
    def asScript: Script = compileToScript(actionF)
    def asScriptString: String = asScript.mkString("\n")
  }

  def containScriptTemplate(lines: String): Matcher[Script] = {
    val checks: Seq[ValueCheck[String]] = lines.split("\n").toSeq.map(_.trim).filterNot(_.isEmpty).map(beMatching(_))
    (_: Script) must ContainWithResultSeq(checks)
  }

  @deprecated("Use containScriptTemplate")
  def beContainedInScript(substring: String): Matcher[ActionF[_]] =
    compileToScript(_: ActionF[_]).mkString("\n") must contain(substring.stripMargin.trim)

  @deprecated("Use containScriptTemplate")
  def beActionAsScript(script: String): Matcher[ActionF[_]] =
    compileToScript(_: ActionF[_]).mkString("\n") must_=== script.stripMargin.trim

  def expectedAction(expectedAction: ActionF[_]): Matcher[ActionF[_]] =
    compileToScript(_: ActionF[_]) must_=== compileToScript(expectedAction)

}
