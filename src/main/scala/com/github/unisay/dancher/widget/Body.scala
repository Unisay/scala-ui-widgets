package com.github.unisay.dancher.widget

import com.github.unisay.dancher.DomBinding
import com.github.unisay.dancher.dom._

case class Body(override val domId: DomId) extends Widget {
  def children: Traversable[Widget] = Seq.empty
  def create = getDocumentBody.map(DomBinding(_))
}
