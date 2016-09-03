package com.github.unisay.dancher

import cats.Eq
import cats.syntax.cartesian._
import scalacheck.cats._
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.interpreter.JsInterpreter.JsInterpreterElement
import org.scalacheck._

object DomArbitraries {

  case class CssClass(value: String) extends AnyVal {
    override def toString = value
  }

  implicit val cssClassEquality: Eq[CssClass] = new Eq[CssClass] {
    def eqv(x: CssClass, y: CssClass) = x == y
  }

  implicit val arbitraryCssClass: Arbitrary[CssClass] = Arbitrary {
    Gen.alphaStr.suchThat(_.nonEmpty).map(CssClass)
  }

  implicit val arbitraryJsInterpreterElement: Arbitrary[JsInterpreterElement] = Arbitrary {
    Gen.numChar.map(index => JsInterpreterElement(s"element$index"))
  }

  implicit val arbitraryPoint: Arbitrary[Vector2d] = Arbitrary {
    (Arbitrary.arbitrary[Double] |@| Arbitrary.arbitrary[Double]).map(Vector2d)
  }

  implicit val arbitraryClickEvent: Arbitrary[ClickEvent] = Arbitrary[ClickEvent] {
    (arbitraryPoint.arbitrary |@| arbitraryPoint.arbitrary |@| arbitraryPoint.arbitrary)
      .map { (_screen: Vector2d, _client: Vector2d, _page: Vector2d) => new ClickEvent {
          override val page   = _page
          override val client = _client
          override val screen = _screen
          override def toString: String = s"ClickEvent(screen=$screen, client=$client, page=$page)"
        }
      }
  }

  implicit val arbitraryMouseMoveEvent: Arbitrary[MouseMoveEvent] = Arbitrary[MouseMoveEvent] {
    (arbitraryPoint.arbitrary |@| arbitraryPoint.arbitrary |@| arbitraryPoint.arbitrary)
      .map { (_screen: Vector2d, _client: Vector2d, _page: Vector2d) => new MouseMoveEvent {
          override val page   = _page
          override val client = _client
          override val screen = _screen
          override def toString: String = s"MouseMoveEvent(screen=$screen, client=$client, page=$page)"
        }
      }
  }

  implicit val arbitraryMouseUpEvent: Arbitrary[MouseUpEvent] = Arbitrary[MouseUpEvent] {
    (arbitraryPoint.arbitrary |@| arbitraryPoint.arbitrary |@| arbitraryPoint.arbitrary)
      .map { (_screen: Vector2d, _client: Vector2d, _page: Vector2d) => new MouseUpEvent {
          override val page   = _page
          override val client = _client
          override val screen = _screen
          override def toString: String = s"MouseUpEvent(screen=$screen, client=$client, page=$page)"
        }
      }
  }

  implicit val arbitraryMouseDownEvent: Arbitrary[MouseDownEvent] = Arbitrary[MouseDownEvent] {
    (arbitraryPoint.arbitrary |@| arbitraryPoint.arbitrary |@| arbitraryPoint.arbitrary)
      .map { (_screen: Vector2d, _client: Vector2d, _page: Vector2d) => new MouseDownEvent {
          override val page   = _page
          override val client = _client
          override val screen = _screen
          override def toString: String = s"MouseDownEvent(screen=$screen, client=$client, page=$page)"
        }
      }
  }

  implicit val arbitraryMouseEnterEvent: Arbitrary[MouseEnterEvent] = Arbitrary[MouseEnterEvent] {
    (arbitraryPoint.arbitrary |@| arbitraryPoint.arbitrary |@| arbitraryPoint.arbitrary)
      .map { (_screen: Vector2d, _client: Vector2d, _page: Vector2d) => new MouseEnterEvent {
          override val page   = _page
          override val client = _client
          override val screen = _screen
          override def toString: String = s"MouseEnterEvent(screen=$screen, client=$client, page=$page)"
        }
      }
  }

  implicit val arbitraryMouseLeaveEvent: Arbitrary[MouseLeaveEvent] = Arbitrary[MouseLeaveEvent] {
    (arbitraryPoint.arbitrary |@| arbitraryPoint.arbitrary |@| arbitraryPoint.arbitrary)
      .map { (_screen: Vector2d, _client: Vector2d, _page: Vector2d) => new MouseLeaveEvent {
          override val page   = _page
          override val client = _client
          override val screen = _screen
          override def toString: String = s"MouseLeaveEvent(screen=$screen, client=$client, page=$page)"
        }
      }
  }

  implicit val arbitraryMouseEvent: Arbitrary[MouseEvent] = Arbitrary {
    Gen.oneOf(
      arbitraryClickEvent.arbitrary,
      arbitraryMouseMoveEvent.arbitrary,
      arbitraryMouseUpEvent.arbitrary,
      arbitraryMouseDownEvent.arbitrary,
      arbitraryMouseEnterEvent.arbitrary,
      arbitraryMouseLeaveEvent.arbitrary
    )
  }

  implicit val arbitraryDomEvent: Arbitrary[DomEvent] = Arbitrary {
    arbitraryMouseEvent.arbitrary
  }

}
