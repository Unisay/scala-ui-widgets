package com.github.unisay.dancher

import com.github.unisay.dancher.dom.{ActionF, DomBinding}

package object widget {
  type RenderAction[N] = ActionF[DomBinding[N]]
}
