package com.github.unisay.dancher.widget

import cats.implicits._
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.interpreter.ActionInterpreter
import com.github.unisay.dancher.widget.RenderAction._
import com.github.unisay.dancher.widget.Widget._
import monix.reactive.Observable

trait LayoutWidgets {

  def Horizontal[M](children: Iterable[Widget[M]],
                    cssClasses: Iterable[String] = Nil,
                    eventTypes: Iterable[DomEventType] = Nil)
                   (implicit interpreter: ActionInterpreter): Widget[M] =
    Div(children, "d-horizontal" :: cssClasses.toList, eventTypes)

  def Vertical[M](children: Iterable[Widget[M]],
                  cssClasses: Iterable[String] = Nil,
                  eventTypes: Iterable[DomEventType] = Nil)
                 (implicit interpreter: ActionInterpreter): Widget[M] =
    Div(children, "d-vertical" :: cssClasses.toList, eventTypes)

  def HorizontalSplit[M](left: Widget[M], right: Widget[M])
                        (implicit interpreter: ActionInterpreter): Widget[M] = {

    val domEventTypes = List(MouseEnter, MouseLeave, MouseMove, MouseUp, MouseDown)
    val leftDiv = Div(List(left), cssClasses = "d-horizontal-split-side" :: "d-horizontal-split-side-left" :: Nil)
    val splitter = Div[M](Nil, cssClasses = "d-horizontal-split-splitter" :: Nil, domEventTypes)
    val rightDiv = Div(List(right), cssClasses = "d-horizontal-split-side" :: "d-horizontal-split-side-right" :: Nil)
    Widget { model: M =>
      Horizontal[M](leftDiv > splitter > rightDiv, cssClasses = "d-horizontal-split" :: Nil).apply(model)
    }
  }


  private def Div[M](children: Iterable[Widget[M]],
                     cssClasses: List[String] = Nil,
                     eventTypes: Iterable[DomEventType] = Nil)
                    (implicit interpreter: ActionInterpreter): Widget[M] = {
    import interpreter._
    Widget { model: M =>
      val divAction: RenderAction = for {
        element <- createElement("div")
        events <- if (eventTypes.isEmpty)
                    value(Observable.empty[(M, DomEventType, DomEventT)])
                  else
                    handleEvents(element, eventTypes)
        _ <- cssClasses.toNel.map(setClasses(element, _)).getOrElse(noAction).void
      } yield DomBinding(element, events0 = events)

      val renderActions = children.map(_(model))
      renderActions.foldLeft(divAction) {
        case (parentAction, childAction) =>
          append(parentAction, childAction)
      }
    }
  }
}

object LayoutWidgets extends LayoutWidgets
