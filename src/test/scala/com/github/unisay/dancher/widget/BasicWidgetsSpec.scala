package com.github.unisay.dancher.widget

import cats.data.Ior
import com.github.unisay.dancher.dom.{DomBinding, DomEvent}
import monix.execution.Scheduler.Implicits.global
import monix.reactive.subjects.ConcurrentSubject
import monix.reactive.{Observer, OverflowStrategy}
import org.scalajs.dom.Element
import org.scalatest.{FlatSpec, MustMatchers}
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import scalatags.generic.{Attr, AttrPair}

class BasicWidgetsSpec extends FlatSpec with MustMatchers with BasicWidgets {

  behavior of "TagWidget"

  it must "render correctly" in {
    val tag = div (
      h1 (id := "title", "This is a title"),
      p ("This is a big paragraph of text"),
      button (onclick := "")
    )

    def enrichEvents[E](observer: Observer[E])(modifiers: List[Seq[Modifier]]): List[Seq[Modifier]] =
      modifiers.map { _.map {
        case pair@AttrPair(Attr("onclick", _, _), "", _) =>
          val cb: E => Unit = { e => observer.onNext(e); () }
          pair.copy(v = cb)
        case mod => mod
      }}

    val events = ConcurrentSubject.publish[DomEvent Ior EffectAction](OverflowStrategy.Unbounded)
    val tag1: TypedTag[Element] = tag.copy(modifiers = enrichEvents[DomEvent Ior EffectAction](events)(tag.modifiers))
    val binding = DomBinding(element = tag1.render, domStream = events)

    println(binding)
  }

}
