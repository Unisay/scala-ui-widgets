package com.github.unisay.dancher.widget

import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.widget.RenderAction._
import cats.syntax.all._

trait LayoutWidgets {

  def Horizontal[M, E: DomElem](children: List[Widget[M]], cssClasses: List[String] = Nil): Widget[M] =
    Div(children, "d-horizontal" :: cssClasses)

  def Vertical[M, E: DomElem](children: List[Widget[M]], cssClasses: List[String] = Nil): Widget[M] =
    Div(children, "d-vertical" :: cssClasses)

  private def Div[M, E: DomElem](children: List[Widget[M]], cssClasses: List[String] = Nil): Widget[M] =
    Widget { (model: M) =>
      val divAction: RenderAction = for {
        element <- createElement("div")
        _ <- cssClasses.toNel.map(setClasses(element, _)).getOrElse(noAction).void
      } yield DomBinding(element)

      val renderActions = children.map(_(model))
      renderActions.foldLeft(divAction) {
        case (parentAction, childAction) =>
          append(parentAction, childAction)
      }
    }
}

object LayoutWidgets extends LayoutWidgets
