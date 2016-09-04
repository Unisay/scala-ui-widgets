package com.github.unisay.dancher.widget

import cats.data.{Ior, Reader}
import cats.syntax.all._
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.widget.Widget._
import monix.reactive.Observable
import monocle.Lens

trait BasicWidgets {

  def Body[E: DomElem, M]: Widget[E, M] =
    Reader(_ => getDocumentBody.map(element => DomBinding(element)))

  def Button[E: DomElem, M](text: Lens[M, String],
                cssClasses: List[String] = Nil,
                eventTypes: Iterable[DomEventType] = Nil): Widget[E, M] =
    TextContainer(
      text = text,
      tag = "button",
      cssClasses = "d-button" :: cssClasses,
      eventTypes = eventTypes
    )

  def Text[E: DomElem, M](text: Lens[M, String], cssClasses: List[String] = Nil): Widget[E, M] =
    TextContainer(text, tag = "p", cssClasses =  "d-text" :: cssClasses)

  def Label[E: DomElem, M](text: Lens[M, String], cssClasses: List[String] = Nil): Widget[E, M] =
    TextContainer(text, tag = "span", cssClasses =  "d-label" :: cssClasses)

  def Header[E: DomElem, M](text: String, size: Int = 1): Widget[E, M] =
    TextContainer(const(text), tag = "h" + size)

  private def TextContainer[E: DomElem, M](text: Lens[M, String],
                                           tag: String,
                                           cssClasses: List[String] = Nil,
                                           eventTypes: Iterable[DomEventType] = Nil): Widget[E, M] = {
    Widget {
      (model: M) => for {
        element ← createElement(tag)
        _ <- appendText(element, text.get(model))
        _ <- cssClasses.toNel.map(setClasses(element, _)).getOrElse(noAction).void
        domEvents <- if (eventTypes.isEmpty) value(Observable.empty[DomEvent Ior EffectAction])
                     else handleEvents(element, eventTypes)
      } yield DomBinding(element, domStream = domEvents)
    }
  }

}

object BasicWidgets extends BasicWidgets
