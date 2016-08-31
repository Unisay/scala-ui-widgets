package com.github.unisay.dancher.widget

import cats.data.Ior
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

    case class Drag(inside: Boolean,
                    dragStart: Option[MouseDownEvent] = None,
                    dragEnd: Option[MouseEvent] = None,
                    event: Option[DomEvent] = None) {
      val isDragging: Boolean = dragStart.nonEmpty
    }

    val domEventTypes = List(MouseEnter, MouseLeave, MouseMove, MouseUp, MouseDown)
    val leftDiv = Div(List(left), cssClasses = "d-horizontal-split-side" :: "d-horizontal-split-side-left" :: Nil)
    val splitter = Div[M](Nil, cssClasses = "d-horizontal-split-splitter" :: Nil, domEventTypes)
    val rightDiv = Div(List(right), cssClasses = "d-horizontal-split-side" :: "d-horizontal-split-side-right" :: Nil)
    val internalWidget = Horizontal[M](leftDiv > splitter > rightDiv, cssClasses = "d-horizontal-split" :: Nil)
    Widget { model: M =>
      internalWidget(model).map { domBinding =>
        val splitterBinding: DomBinding = domBinding.nested(1)
        domBinding.mapDomStream { _ =>
          splitterBinding.domStream.scan(Drag(inside = false)) { // TODO: what if inside is true?
            case (drag @ Drag(_, _, _, _), Ior.Left(event: MouseMoveEvent)) =>
              drag.copy(event = Some(event))
            case (drag @ Drag(false, _, _, _), Ior.Left(event: MouseEnterEvent)) =>
              drag.copy(inside = true, event = Some(event))
            case (drag @ Drag(true, _, _, _), Ior.Left(event: MouseLeaveEvent)) =>
              drag.copy(inside = false, event = Some(event))
            case (drag @ Drag(true, None, _, _), Ior.Left(event: MouseDownEvent)) =>
              drag.copy(dragStart = Some(event), event = Some(event))
            case (drag @ Drag(_, Some(_), _, _), Ior.Left(event: MouseUpEvent)) =>
              drag.copy(dragEnd = Some(event), event = Some(event))
            case (drag @ Drag(_, None, Some(_), _), _) =>
              drag.copy(dragEnd = None)
            case (drag, _) =>
              drag
          }
            .filter(_.isDragging)
            .map(_.event.get)

        }
      }
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
        events <- if (eventTypes.isEmpty) value(Observable.empty) else handleEvents(element, eventTypes)
        _ <- cssClasses.toNel.map(setClasses(element, _)).getOrElse(noAction).void
      } yield DomBinding(element, domStream = events)

      val renderActions = children.map(_(model))
      renderActions.foldLeft(divAction) {
        case (parentAction, childAction) =>
          append(parentAction, childAction)
      }
    }
  }
}

object LayoutWidgets extends LayoutWidgets
