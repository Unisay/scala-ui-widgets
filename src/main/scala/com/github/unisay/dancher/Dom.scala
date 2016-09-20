package com.github.unisay.dancher

object Dom {

  object Event {
    sealed trait Type { val name = this.getClass.getSimpleName.toLowerCase }
    sealed trait MouseEventType extends Type
    case object Click extends MouseEventType
    case object MouseMove extends MouseEventType
    case object MouseUp extends MouseEventType
    case object MouseDown extends MouseEventType
    case object MouseEnter extends MouseEventType
    case object MouseLeave extends MouseEventType
  }


}
