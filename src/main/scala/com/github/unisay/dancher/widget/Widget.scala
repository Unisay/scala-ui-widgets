package com.github.unisay.dancher.widget

import com.github.unisay.dancher.dom._

import scala.language.implicitConversions

// TODO: structure packages like in cats
// TODO: generate Is* using macros? >>> Lib

trait SomeWidget {
  type W
  val instance: W
  val widget: Widget[W]
  val container: Option[Container[W]]
  def domId = widget.domId(instance)
  def create = widget.create(instance)
  def remove = widget.remove(instance)
}

object SomeWidget {
  implicit def isWidget[W: Widget](w: W): SomeWidget = SomeWidget(w)
  def apply[W0](w0: W0)(implicit widgetEvidence: Widget[W0], containerEvidence: Container[W0] = null): SomeWidget =
    new SomeWidget {
      type W = W0
      val instance = w0
      val widget = widgetEvidence
      val container = Option(containerEvidence)
      override def hashCode(): Int = instance.hashCode()
      override def equals(obj: scala.Any): Boolean =
        obj.isInstanceOf[SomeWidget] && instance.equals(obj.asInstanceOf[SomeWidget].instance)
      override def toString: String = s"SomeWidget($instance)"
    }
}

trait Widget[W] {

  /** @return current DOM id */
  def domId(w: W): DomId

  /** @return current DOM element */
  def element(w: W): ActionF[DomElement] = getElementById(domId(w))

  /** @return current DOM node */
  def node(w: W): ActionF[DomNode] = element(w).map(element ⇒ element: DomNode)

  /** @return parent node */
  def parent(w: W): ActionF[DomNode] = element(w).flatMap(getParentNode)

  /** @return first child node */
  def firstChild(w: W): ActionF[DomNode] = element(w).flatMap(getFirstChild)

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
  def replaceWith[B : Widget](w: W)(that: B): ActionF[DomNode] =
    for {
      oldChild ← element(w)
      parent ← oldChild.getParent
      newChild ← implicitly[Widget[B]].create(that)
      _ ← parent.replaceChild(newChild, oldChild)
    } yield oldChild

}

object Widget {

  implicit class WidgetOps[W: Widget](widget: W) {
    def element: ActionF[DomElement] = implicitly[Widget[W]].element(widget)
  }

}
