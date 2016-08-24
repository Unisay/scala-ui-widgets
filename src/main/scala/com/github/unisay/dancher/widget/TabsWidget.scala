package com.github.unisay.dancher.widget

import com.github.unisay.dancher._
import com.github.unisay.dancher.dom._
import monix.reactive.Observable
import monocle._

trait TabsWidget extends BasicWidgets with LayoutWidgets with WidgetSyntax {

  case class TabActivated[M](index: Int) extends DomainEvent

  def Tabs[M](activeTabIndexLens: Lens[M, Int])(children: (String, Widget[M])*): Widget[M] = new Widget[M] {

    val _children = children.toList

    def render(model: M): RenderAction[M] = {
      val activeTabIndex = Math.max(Math.min(activeTabIndexLens.get(model), children.length), 0)


      val buttons = _children.map(_._1).zipWithIndex.map { case (text, index) =>
        Button[M](const(text), clickHandler = Some { _ =>
          val updatedModel = activeTabIndexLens.set(index)(model)
          Observable(ModelEvent(updatedModel, TabActivated(index)))
        },
          cssClasses = "d-tab" :: (if (index == activeTabIndex) List("d-tab-active") else Nil))
      }

      val hiddenChildWidgets = _children.map(_._2).zipWithIndex.map {
        case (widget, index) if index != activeTabIndex =>
          createRender {
            for {
              domBinding <- widget.render(model)
              hiddenElement <- domBinding.element.hide
            } yield domBinding.copy(element = hiddenElement)
          }
        case (widget, _) =>
          widget
      }

      Vertical(
        Horizontal(buttons) >
        Vertical(hiddenChildWidgets)
      ).render(model)
    }

    override def update(model: M, event: DomainEvent): (M, ActionF[Unit]) = {
      super.update(model, event)
    }
  }
}

object TabsWidget extends TabsWidget
