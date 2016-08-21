package com.github.unisay.dancher

import cats.data.Ior
import cats.syntax.functor._
import com.github.unisay.dancher.RenderAction._
import com.github.unisay.dancher.dom._
import monix.reactive.Observable
import monocle.Lens


object RenderAction {
  type RenderAction[M] = ActionF[DomBinding[M]]

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

trait Widget[M] {
  def render(model: M): RenderAction[M]
  def update(model: M, event: DomainEvent): (M, ActionF[Unit]) = (model, noAction)
}

trait WidgetSyntax {
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


object Widget extends WidgetSyntax {

  def const[M, C](constant: C): Lens[M, C] = Lens[M, C](_ => constant)(_ => identity)

  def createRender[M](action: RenderAction[M]) = new Widget[M] {
    def render(model: M) = action
  }

  def Body[M] = createRender(getDocumentBody.map(DomBinding[M](_)))

  def TextContainer[M](text: Lens[M, String], tag: String,
                       clickHandler: Option[DomEventHandler[M]] = None,
                       cssClasses: List[String] = Nil): Widget[M] = new Widget[M] {
      def render(model: M): ActionF[DomBinding[M]] =
        for {
          element ← createElement(tag)
          events <- clickHandler.fold { value[Option[Observable[M Ior DomainEvent]]](None) }
                                      { element.onClick(_).map(Some(_)) }
          _ <- if (cssClasses.isEmpty) noAction else element.setClasses(cssClasses).void
          _ <- element appendText text.get(model)
        } yield DomBinding(element, events.getOrElse(Observable.empty))
    }

  def Button[M](text: String,
                clickHandler: Option[DomEventHandler[M]] = None,
                cssClasses: List[String] = Nil): Widget[M] =
    TextContainer(
      text = const(text),
      tag = "button",
      cssClasses = "d-button" :: cssClasses,
      clickHandler = clickHandler
    )

  def Label[M](text: Lens[M, String], cssClasses: List[String] = Nil): Widget[M] =
    TextContainer(text, tag = "span", cssClasses =  "d-label" :: cssClasses)

  def Header[M](text: String, size: Int = 1): Widget[M] =
    TextContainer(const(text), tag = "h" + size)


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

  def Horizontal[M](children: List[Widget[M]], cssClasses: List[String] = Nil): Widget[M] =
    Div(children, "d-horizontal" :: cssClasses)

  def Vertical[M](children: List[Widget[M]], cssClasses: List[String] = Nil): Widget[M] =
    Div(children, "d-vertical" :: cssClasses)


  case class TabActivated[M](index: Int) extends DomainEvent

  def Tabs[M](activeTabIndexLens: Lens[M, Int])(children: (String, Widget[M])*): Widget[M] = new Widget[M] {
    def render(model: M): RenderAction[M] = {
      val activeTabIndex = Math.max(Math.min(activeTabIndexLens.get(model), children.length), 0)
      val (buttonTexts, childWidgets) = children.toList.unzip

      val buttons = buttonTexts.zipWithIndex.map { case (text, index) =>
        Button[M](text, clickHandler = Some { _ =>
            val updatedModel = activeTabIndexLens.set(index)(model)
            Observable(Ior.Both(updatedModel, TabActivated(index)))
          },
          cssClasses = "d-tab" :: (if (index == activeTabIndex) List("d-tab-active") else Nil))
      }

      val hiddenChildWidgets = childWidgets.zipWithIndex.map {
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
