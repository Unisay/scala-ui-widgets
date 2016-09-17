package com.github.unisay.dancher.widget

import cats.data.Ior
import cats.implicits._
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.widget.RenderAction._
import com.github.unisay.dancher.widget.Widget._
import monix.reactive.Observable
import monix.reactive.Observable.merge
import org.scalajs.dom.Element
import com.github.unisay.dancher.CssSyntax._

trait LayoutWidgets {

  def Horizontal[M](children: Iterable[Widget[M]],
                    cssClasses: Iterable[String] = Nil,
                    eventTypes: Iterable[DomEventType] = Nil): Widget[M] =
    Div (
      children = children,
      cssClasses = "d-horizontal" :: cssClasses.toList,
      eventTypes = eventTypes
    )

  def Vertical[M](children: Iterable[Widget[M]],
                  cssClasses: Iterable[String] = Nil,
                  eventTypes: Iterable[DomEventType] = Nil): Widget[M] =
    Div (
      children = children,
      cssClasses = "d-vertical" :: cssClasses.toList,
      eventTypes = eventTypes
    )

  def HorizontalSplit[M](left: Widget[M], right: Widget[M]): Widget[M] = {

    case class Drag(inside: Boolean,
                    initWidth: Option[Int] = None,
                    start: Option[Vector2d] = None,
                    current: Option[Vector2d] = None,
                    end: Option[Vector2d] = None)

    val leftDiv = Div(
      children = List(left),
      attributes = List("draggable" -> false.toString),
      cssClasses = "d-horizontal-split-side" :: "d-horizontal-split-side-left" :: Nil)
    val splitter = Div[M](
      children = Nil,
      attributes = List("draggable" -> false.toString),
      cssClasses = "d-horizontal-split-splitter" :: Nil,
      eventTypes = List(MouseEnter, MouseLeave, MouseMove, MouseUp, MouseDown))
    val rightDiv = Div(
      children = List(right),
      attributes = List("draggable" -> false.toString),
      cssClasses = "d-horizontal-split-side" :: "d-horizontal-split-side-right" :: Nil)
    val internalWidget = Horizontal[M](
      children = leftDiv > splitter > rightDiv,
      cssClasses = "d-horizontal-split" :: Nil,
      eventTypes = List(MouseMove, MouseUp, MouseDown))

    def splitterDomStream(domStream: DomStream, element: Element): DomStream =
      domStream.scan(Drag(inside = false)) { // TODO: what if inside is true?
        case (drag@Drag(_, _, Some(_), _, _), Ior.Left(event: MouseMoveEvent)) =>
          println(drag)
          drag.copy(current = Some(event.screen))
        case (drag@Drag(false,_,  _, _, _), Ior.Left(event: MouseEnterEvent)) =>
          println(drag)
          drag.copy(inside = true, current = Some(event.screen))
        case (drag@Drag(true,_,  _, _, _), Ior.Left(event: MouseLeaveEvent)) =>
          println(drag)
          drag.copy(inside = false, current = Some(event.screen))
        case (drag@Drag(true,_,  None, _, _), Ior.Left(event: MouseDownEvent)) =>
          println(drag)
          drag.copy(start = Some(event.screen), current = Some(event.screen), initWidth = Some(element.clientWidth))
        case (drag@Drag(_,_,  Some(_), _, _), Ior.Left(event: MouseUpEvent)) =>
          println(drag)
          drag.copy(end = Some(event.screen), current = Some(event.screen))
        case (drag@Drag(_, _, Some(_), _, Some(_)), _) =>
          drag.copy(start = None, end = None)
        case (drag, _) =>
          drag
      }
      .filter(drag => drag.start.isDefined && drag.end.isEmpty)
      .flatMap { drag =>
        Observable.fromIterable {
          for {
            start <- drag.start
            curr  <- drag.current
            width <- drag.initWidth
          } yield Math.max(Math.round(width - start.x + curr.x).toInt, 1)
        }
      }
      .map(width => setAttribute("style", "width: " + width.px)(element).void)
      .map(Ior.Right(_))


    Widget {
      internalWidget(_).map { binding =>
        binding.mapDomStream { _ =>
          val leftDivBinding = binding.nested(0)
          val splitterBinding = binding.nested(1)
          val merged = merge(splitterBinding.domStream, binding.domStream)
          splitterDomStream(merged, leftDivBinding.element)
        }
      }
    }
  }

  private def Div[M](children: Iterable[Widget[M]],
                     attributes: List[(String, String)] = Nil,
                     cssClasses: List[String] = Nil,
                     eventTypes: Iterable[DomEventType] = Nil): Widget[M] = {
    Widget { model: M =>
      val divAction: RenderAction[M] = for {
        element <- createElement("div")
        events <- if (eventTypes.isEmpty) value(Observable.empty) else handleEvents(element, eventTypes)
        _ <- cssClasses.toNel.map(setClasses(element, _)).getOrElse(noAction).void
        _ <- attributes.toNel.map(_.traverse(t => setAttribute(t._1, t._2)(element))).getOrElse(noAction).void
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
