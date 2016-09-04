package com.github.unisay.dancher

import cats.data.Reader
import com.github.unisay.dancher.dom.{ActionF, DomBinding}

package object widget {
  type EffectAction = ActionF[Unit]
  type RenderAction[E, M] = ActionF[DomBinding[E, M]]
  type Widget[E, M] = M Reader RenderAction[E, M]
}
