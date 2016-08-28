package com.github.unisay.dancher.widget

import cats.data.Reader
import cats.syntax.all._
import com.github.unisay.dancher.dom.DomEventHandlers.NoHandlers
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.interpreter.ActionInterpreter
import com.github.unisay.dancher.widget.Widget._
import monix.reactive.Observable
import monocle.Lens

trait BasicWidgets {

  def Body[M, E: DomElem]: Widget[M] = Reader(_ => getDocumentBody.map(e => DomBinding(e)))

  def Button[M](text: Lens[M, String],
                eventHandlers: DomEventHandlers[M] = NoHandlers[M],
                cssClasses: List[String] = Nil)
               (implicit interpreter: ActionInterpreter): Widget[M] =
    TextContainer(
      text = text,
      tag = "button",
      cssClasses = "d-button" :: cssClasses,
      eventHandlers = eventHandlers
    )

  def Text[M](text: Lens[M, String], cssClasses: List[String] = Nil)
             (implicit interpreter: ActionInterpreter): Widget[M] =
    TextContainer(text, tag = "p", cssClasses =  "d-text" :: cssClasses)

  def Label[M](text: Lens[M, String], cssClasses: List[String] = Nil)
              (implicit interpreter: ActionInterpreter): Widget[M] =
    TextContainer(text, tag = "span", cssClasses =  "d-label" :: cssClasses)

  def Header[M](text: String, size: Int = 1)
               (implicit interpreter: ActionInterpreter): Widget[M] =
    TextContainer(const(text), tag = "h" + size)

  private def TextContainer[M](text: Lens[M, String], tag: String,
                               eventHandlers: DomEventHandlers[M] = NoHandlers[M],
                               cssClasses: List[String] = Nil)
                              (implicit interpreter: ActionInterpreter): Widget[M] = {
    import interpreter._
    Widget {
      (model: M) => for {
        element ← createElement(tag)
        _ <- appendText(element, text.get(model))
        _ <- cssClasses.toNel.map(setClasses(element, _)).getOrElse(noAction).void
        events <- if (eventHandlers.isEmpty) value(Observable.empty) else handleEvents(element, eventHandlers)
      } yield DomBinding(element, Vector.empty, events)
    }
  }

}

object BasicWidgets extends BasicWidgets
