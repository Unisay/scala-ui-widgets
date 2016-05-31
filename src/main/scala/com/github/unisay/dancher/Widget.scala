package com.github.unisay.dancher

import dom._

sealed abstract class Widget(val domId: DomId) {

  /** Returns current DOM element */
  def element: ActionF[DomElement] = getElementById(domId.value)

  /** Returns parent node */
  def parent: ActionF[DomNode] =
    for {
      child ← element
      parent ← child.getParent
    } yield parent

  /** Creates model and returns its topmost DOM element (root) */
  def create: ActionF[DomElement]

  /** Removes current model from it's parent DOM node and returns parent */
  def remove: ActionF[DomNode] =
    for {
      child ← element
      parent ← child.getParent
      _ ← parent removeChild child
    } yield parent

  /** Replaces current element by other element in the parent returning old element */
  def replaceWith(that: Widget): ActionF[DomNode] =
    for {
      oldChild ← this.element
      parent ← oldChild.getParent
      newChild ← that.create
      _ ← parent.replaceChild(oldChild, newChild)
    } yield oldChild

}

abstract class LeafWidget(override val domId: DomId) extends Widget(domId)
abstract class NodeWidget(override val domId: DomId) extends Widget(domId) {
  def children: Traversable[Widget]
}
