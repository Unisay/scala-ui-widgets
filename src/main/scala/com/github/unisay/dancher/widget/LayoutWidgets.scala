package com.github.unisay.dancher.widget

import com.github.unisay.dancher.dom._

trait LayoutWidgets extends RenderActionOps {

  def Horizontal[M](children: List[Widget[M]], cssClasses: List[String] = Nil): Widget[M] =
    Div(children, "d-horizontal" :: cssClasses)

  def Vertical[M](children: List[Widget[M]], cssClasses: List[String] = Nil): Widget[M] =
    Div(children, "d-vertical" :: cssClasses)

  private def Div[M](children: List[Widget[M]], cssClasses: List[String] = Nil): Widget[M] = new Widget[M] {
    def render(model: M): RenderAction[M] = {
      val divAction: RenderAction[M] = createElement("div").flatMap(_.setClasses(cssClasses)).map(DomBinding(_))
      val renderActions = children.map(_.render(model))
      renderActions.foldLeft(divAction) {
        case (parentAction, childAction) =>
          appendReturningParent(parentAction, childAction)
      }
    }
  }
}

object LayoutWidgets extends LayoutWidgets
