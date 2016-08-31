package com.github.unisay.dancher

import cats.Eq
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

  implicit val arbitraryClickEvent: Arbitrary[ClickEvent] = Arbitrary[ClickEvent] {
    Gen.numChar.map(index => new ClickEvent { override def toString: String = s"ClickEvent($index)" })
  }

  implicit val arbitraryMouseMoveEvent: Arbitrary[MouseMoveEvent] = Arbitrary[MouseMoveEvent] {
    Gen.numChar.map(index => new MouseMoveEvent { override def toString: String = s"MouseMoveEvent($index)" })
  }
  
  implicit val arbitraryMouseUpEvent: Arbitrary[MouseUpEvent] = Arbitrary[MouseUpEvent] {
    Gen.numChar.map(index => new MouseUpEvent { override def toString: String = s"MouseUpEvent($index)" })
  }
    
  implicit val arbitraryMouseDownEvent: Arbitrary[MouseDownEvent] = Arbitrary[MouseDownEvent] {
    Gen.numChar.map(index => new MouseDownEvent { override def toString: String = s"MouseDownEvent($index)" })
  }
    
  implicit val arbitraryMouseEnterEvent: Arbitrary[MouseEnterEvent] = Arbitrary[MouseEnterEvent] {
    Gen.numChar.map(index => new MouseEnterEvent { override def toString: String = s"MouseEnterEvent($index)" })
  }
    
  implicit val arbitraryMouseLeaveEvent: Arbitrary[MouseLeaveEvent] = Arbitrary[MouseLeaveEvent] {
    Gen.numChar.map(index => new MouseLeaveEvent { override def toString: String = s"MouseLeaveEvent($index)" })
  }

  implicit val arbDomEvent: Arbitrary[DomEvent] = Arbitrary {
    Gen.oneOf(
      arbitraryClickEvent.arbitrary,
      arbitraryMouseMoveEvent.arbitrary,
      arbitraryMouseUpEvent.arbitrary,
      arbitraryMouseDownEvent.arbitrary,
      arbitraryMouseEnterEvent.arbitrary,
      arbitraryMouseLeaveEvent.arbitrary
    )
  }

}
