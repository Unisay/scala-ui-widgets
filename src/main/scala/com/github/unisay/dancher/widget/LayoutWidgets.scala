package com.github.unisay.dancher.widget

import cats.implicits._
import com.github.unisay.dancher.dom.DomEventHandlers._
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.interpreter.ActionInterpreter
import com.github.unisay.dancher.widget.RenderAction._
import com.github.unisay.dancher.widget.Widget._
import monix.reactive.Observable

trait LayoutWidgets {

  def Horizontal[M](children: List[Widget[M]],
                    cssClasses: List[String] = Nil,
                    eventHandlers: DomEventHandlers[M] = NoHandlers[M])
                   (implicit interpreter: ActionInterpreter): Widget[M] =
    Div(children, "d-horizontal" :: cssClasses, eventHandlers)

  def Vertical[M](children: List[Widget[M]],
                  cssClasses: List[String] = Nil,
                  eventHandlers: DomEventHandlers[M] = NoHandlers[M])
                 (implicit interpreter: ActionInterpreter): Widget[M] =
    Div(children, "d-vertical" :: cssClasses, eventHandlers)

  def HorizontalSplit[M](left: Widget[M], right: Widget[M])
                        (implicit interpreter: ActionInterpreter): Widget[M] = {
    val splitterEventHandlers = On(MouseDown) { (eventModel: M, _) =>
      Observable(HandlerResult(eventModel, log("MouseDown")))
    } ++ On(MouseUp) { (eventModel: M, _) =>
      Observable(HandlerResult(eventModel, log("MouseUp")))
    } ++ On(MouseEnter) { (eventModel: M, _) =>

      Observable(HandlerResult(eventModel, log("MouseEnter")))
    } ++ On(MouseLeave) { (eventModel: M, _) =>
      Observable(HandlerResult(eventModel, log("MouseLeave")))
    } ++ On(MouseMove) { (eventModel: M, _) =>
      Observable(HandlerResult(eventModel, log("MouseMove")))
    }

    val leftDiv = Div(List(left), cssClasses = "d-horizontal-split-side" :: "d-horizontal-split-side-left" :: Nil)
    val splitter = Div[M](Nil, cssClasses = "d-horizontal-split-splitter" :: Nil, eventHandlers = splitterEventHandlers)
    val rightDiv = Div(List(right), cssClasses = "d-horizontal-split-side" :: "d-horizontal-split-side-right" :: Nil)
    Widget { model: M =>
      Horizontal[M](leftDiv > splitter > rightDiv, cssClasses = "d-horizontal-split" :: Nil).apply(model)
    }
  }


  private def Div[M](children: List[Widget[M]],
                     cssClasses: List[String] = Nil,
                     eventHandlers: DomEventHandlers[M] = NoHandlers[M])
                    (implicit interpreter: ActionInterpreter): Widget[M] = {
    import interpreter._
    Widget { model: M =>
      val divAction: RenderAction = for {
        element <- createElement("div")
        events <- if (eventHandlers.isEmpty) value(Observable.empty) else handleEvents(element, eventHandlers)
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
