package com.github.unisay.dancher.widget

import com.github.unisay.dancher.DomainEvent
import com.github.unisay.dancher.dom._
import monix.reactive.Observable
import monocle.Lens

trait Widget[M] {
  def render(model: M): RenderAction[M]
  def update(model: M, event: DomainEvent): (M, ActionF[Unit]) = (model, noAction)
}

trait RenderActionOps {

  def appendReturningChild[M](parent: RenderAction[M], child: RenderAction[M]): RenderAction[M] =
    for {
      parentBinding <- parent
      childBinding  <- child
      _ <- parentBinding.element.appendChild(childBinding.element)
    } yield DomBinding(childBinding.element, Observable.merge(parentBinding.events, childBinding.events))

  def appendReturningParent[M](parent: RenderAction[M], child: RenderAction[M]): RenderAction[M] =
    for {
      parentBinding <- parent
      childBinding  <- child
      parentElement <- parentBinding.element.appendChild(childBinding.element)
    } yield DomBinding(parentElement, Observable.merge(parentBinding.events, childBinding.events))


  def hide[M](action: RenderAction[M]) =
    for { domBinding <- action; hiddenElement <- domBinding.element.hide }
      yield DomBinding(hiddenElement, domBinding.events)
}

trait WidgetSyntax extends RenderActionOps {

  implicit class WidgetSyntax[M](widget: Widget[M]) {
    def >(other: Widget[M]): List[Widget[M]] = widget :: other :: Nil

    /** Append child to parent returning parent */
    def +>(child: Widget[M]): Widget[M] = new Widget[M] {
      def render(model: M): RenderAction[M] = appendReturningParent(widget.render(model), child.render(model))
    }
  }

  implicit class WidgetListSyntax[M](widgets: List[Widget[M]]) {
    def >(widget: Widget[M]): List[Widget[M]] = widgets :+ widget
  }
}

object Widget extends WidgetSyntax with RenderActionOps

trait WidgetHelpers {
  def const[M, C](constant: C): Lens[M, C] = Lens[M, C](_ => constant)(_ => identity)

  def createRender[M](action: RenderAction[M]) = new Widget[M] {
    def render(model: M) = action
  }
}

object all
  extends WidgetSyntax
  with BasicWidgets
  with LayoutWidgets
  with TabsWidget
