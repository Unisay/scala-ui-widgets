package com.github.unisay.dancher.widget

import cats.data.Ior
import cats.syntax.functor._
import com.github.unisay.dancher.DomainEvent
import com.github.unisay.dancher.dom._
import monix.reactive.Observable
import monocle.Lens

trait BasicWidgets extends WidgetHelpers {

  def Body[M] = createRender(getDocumentBody.map(DomBinding[M](_)))

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

  private def TextContainer[M](text: Lens[M, String], tag: String,
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
}

object BasicWidgets extends BasicWidgets
