package com.github.unisay.dancher

import cats.Eq
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

/*  implicit val arbDomEvent: Arbitrary[DomEvent] = Arbitrary {

  }*/

}
