package com.github.unisay.dancher

import dom._

sealed abstract class Widget(val domId: DomId) {

  /** Returns current DOM element */
  def element: ActionF[DomElement] = getElementById(domId)

  /** Returns current DOM node */
  def node: ActionF[DomNode] = getElementById(domId).map(element ⇒ element: DomNode)

  /** @return parent node */
  def parent: ActionF[DomNode] = element.flatMap(getParentNode)

  /** @return first child node */
  def firstChild: ActionF[DomNode] = element.flatMap(getFirstChild)

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
      _ ← parent.replaceChild(newChild, oldChild)
    } yield oldChild

}

abstract class LeafWidget(override val domId: DomId) extends Widget(domId)
abstract class NodeWidget(override val domId: DomId) extends Widget(domId) {
  def children: Traversable[Widget]
}
