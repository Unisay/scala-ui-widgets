package com.github.unisay.dancher.interpreter

import com.github.unisay.dancher.dom.ActionF

trait ActionInterpreter {

  def interpret[R, M](model: M, action: ActionF[R]): R

}
