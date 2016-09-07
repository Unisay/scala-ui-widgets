package com.github.unisay.dancher

import cats.data.Reader
import com.github.unisay.dancher.dom.{ActionF, DomBinding}

package object widget {
  type EffectAction = ActionF[Unit]
  type RenderAction[M] = ActionF[DomBinding[M]]
  type Widget[M] = M Reader RenderAction[M]
}
