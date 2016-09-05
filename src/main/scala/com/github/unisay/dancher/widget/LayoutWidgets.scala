package com.github.unisay.dancher.widget

import cats.data.Ior
import cats.implicits._
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.widget.RenderAction._
import com.github.unisay.dancher.widget.Widget._
import monix.reactive.Observable
import monix.reactive.Observable.merge

trait LayoutWidgets {

  def Horizontal[E: DomElem, M](children: Iterable[Widget[E, M]],
                    cssClasses: Iterable[String] = Nil,
                    eventTypes: Iterable[DomEventType] = Nil): Widget[E, M] =
    Div (
      children = children,
      cssClasses = "d-horizontal" :: cssClasses.toList,
      eventTypes = eventTypes
    )

  def Vertical[E: DomElem, M](children: Iterable[Widget[E, M]],
                  cssClasses: Iterable[String] = Nil,
                  eventTypes: Iterable[DomEventType] = Nil): Widget[E, M] =
    Div (
      children = children,
      cssClasses = "d-vertical" :: cssClasses.toList,
      eventTypes = eventTypes
    )

  def HorizontalSplit[E: DomElem, M](left: Widget[E, M], right: Widget[E, M]): Widget[E, M] = {

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
    val splitter = Div[E, M](
      children = Nil,
      attributes = List("draggable" -> false.toString),
      cssClasses = "d-horizontal-split-splitter" :: Nil,
      eventTypes = List(MouseEnter, MouseLeave, MouseMove, MouseUp, MouseDown))
    val rightDiv = Div(
      children = List(right),
      attributes = List("draggable" -> false.toString),
      cssClasses = "d-horizontal-split-side" :: "d-horizontal-split-side-right" :: Nil)
    val internalWidget = Horizontal[E, M](
      children = leftDiv > splitter > rightDiv,
      cssClasses = "d-horizontal-split" :: Nil,
      eventTypes = List(MouseMove, MouseUp, MouseDown))

    def moveSplitter(leftDivElement: E)(delta: Vector2d)(implicit elementEvidence: DomElem[E]): EffectAction = {
      val width = elementEvidence.clientWidth(leftDivElement)
      log(s"$width")
    }

    def splitterDomStream(domStream: DomStream, element: E): DomStream =
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


  private def Div[E: DomElem, M](children: Iterable[Widget[E, M]],
                                 attributes: List[(String, String)] = Nil,
                                 cssClasses: List[String] = Nil,
                                 eventTypes: Iterable[DomEventType] = Nil): Widget[E, M] = {
    Widget { model: M =>
      val ie = implicitly[DomElem[E]]
      val divAction: RenderAction[E, M] = for {
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
