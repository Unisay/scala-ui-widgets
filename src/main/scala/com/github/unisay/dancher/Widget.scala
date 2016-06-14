package com.github.unisay.dancher

import dom._
import simulacrum.typeclass

// TODO: structure packages like in cats
// TODO: use simulacrum

@typeclass trait Widget[W] {

  /** @return current DOM id */
  def domId(w: W): DomId

  /** @return current DOM element */
  def element(w: W): ActionF[DomElement] = getElementById(domId(w))

  /** @return current DOM node */
  def node(w: W): ActionF[DomNode] = element(w) map (element ⇒ element: DomNode)

  /** @return parent node */
  def parent(w: W): ActionF[DomNode] = element(w) flatMap getParentNode

  /** @return first child node */
  def firstChild(w: W): ActionF[DomNode] = element(w) flatMap getFirstChild

  /** Creates model and returns its topmost DOM element (root) */
  def create(w: W): ActionF[DomElement]

  /** Removes current model from it's parent DOM node and returns parent */
  def remove(w: W): ActionF[DomNode] =
    for {
      child ← element(w)
      parent ← child.getParent
      _ ← parent removeChild child
    } yield parent

  /** Replaces current element by other element in the parent returning old element */
  def replaceWith[B : Widget](w: W, that: B): ActionF[DomNode] =
    for {
      oldChild ← element(w)
      parent ← oldChild.getParent
      newChild ← implicitly[Widget[B]].create(that)
      _ ← parent.replaceChild(newChild, oldChild)
    } yield oldChild

}

object Widget {

  type WidgetAction[A] = (this.type, ActionF[A])

  implicit class WidgetOps[T](t: T)(implicit ev: Widget[T]) {
    def element: ActionF[DomElement] = ev.element(t)
  }

}
