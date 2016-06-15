package com.github.unisay.dancher.widget

import com.github.unisay.dancher.dom._

trait Container[P] {

  def children(parent: P): Vector[SomeWidget]
  def setChildren(parent: P)(children: Vector[SomeWidget]): P
  def createChildren(parent: P): Vector[ActionF[DomElement]] = children(parent).map(_.create)
  def removeChild(parent: P)(id: DomId): Option[(P, ActionF[DomNode])] = {
    val find: Option[SomeWidget] = children(parent).find(_.domId == id)
    find.map[(P, ActionF[DomNode])]{ childToRemove ⇒
    /* val newChildren = children(parent).filterNot(_ == childToRemove)*/
      val tuple: (P, ActionF[DomNode]) = (parent, childToRemove.remove)
      tuple
    }
  }

}

object Container {

  implicit class ContainerOps[A: Container](a: A) {
    def children: Vector[SomeWidget] = implicitly[Container[A]].children(a)
  }

}
