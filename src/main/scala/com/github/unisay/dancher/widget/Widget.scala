package com.github.unisay.dancher.widget

import com.github.unisay.dancher.DomBinding
import com.github.unisay.dancher.dom.{DomElement, DomNode, _}

trait Widget {

  /** DOM id */
  val domId: DomId

  /** @return current DOM element */
  def element: ActionF[DomElement] = getElementById(domId)

  /** @return current DOM node */
  def node: ActionF[DomNode] = getElementById(domId).map(element ⇒ element: DomNode)

  /** @return parent node */
  def parent: ActionF[DomNode] = element.flatMap(getParentNode)

  /** @return first child node */
  def firstChild: ActionF[DomNode] = element.flatMap(getFirstChild)

  /** Creates model and returns its binding */
  def create: ActionF[DomBinding]

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
      created ← that.create
      _ ← parent.replaceChild(created.element, oldChild)
    } yield oldChild

}
