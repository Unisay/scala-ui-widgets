package com.github.unisay.dancher.widget

import cats.data.Ior
import cats.implicits._
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.widget.RenderAction._
import com.github.unisay.dancher.widget.Widget._
import monix.reactive.Observable
import monix.reactive.Observable.merge
import org.scalajs.dom.Element

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
                    dragStart: Option[Vector2d] = None,
                    dragPos:   Option[Vector2d] = None,
                    dragEnd:   Option[Vector2d] = None) {
      def dragDelta = for {
        start <- Observable.fromIterable(dragStart)
        pos   <- Observable.fromIterable(dragPos)
      } yield start - pos
    }

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

    def moveSplitter(leftDivElement: Element)(delta: Vector2d): EffectAction = {
      val width = leftDivElement.clientWidth
      log(s"$width")
    }

    def splitterDomStream(domStream: DomStream, element: Element): DomStream =
      domStream.scan(Drag(inside = false)) { // TODO: what if inside is true?
        case (drag@Drag(_, Some(_), _, _), Ior.Left(event: MouseMoveEvent)) =>
          drag.copy(dragPos = Some(event.screen))
        case (drag@Drag(false, _, _, _), Ior.Left(event: MouseEnterEvent)) =>
          drag.copy(inside = true, dragPos = Some(event.screen))
        case (drag@Drag(true, _, _, _), Ior.Left(event: MouseLeaveEvent)) =>
          drag.copy(inside = false, dragPos = Some(event.screen))
        case (drag@Drag(true, None, _, _), Ior.Left(event: MouseDownEvent)) =>
          drag.copy(dragStart = Some(event.screen), dragPos = Some(event.screen))
        case (drag@Drag(_, Some(_), _, _), Ior.Left(event: MouseUpEvent)) =>
          drag.copy(dragEnd = Some(event.screen), dragPos = Some(event.screen))
        case (drag@Drag(_, Some(_), Some(_), _), _) =>
          drag.copy(dragStart = None, dragEnd = None)
        case (drag, _) =>
          drag
      }
//      .map(drag => {println(drag); drag})
      .flatMap(_.dragDelta)
      .map(moveSplitter(element))
      .map(Ior.Right.apply)

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
