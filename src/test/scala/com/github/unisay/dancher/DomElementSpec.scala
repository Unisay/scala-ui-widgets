package com.github.unisay.dancher

import cats.data.NonEmptyList
import cats.syntax.eq._
import com.github.unisay.dancher.ActionTestHelpers._
import com.github.unisay.dancher.Arbitraries._
import com.github.unisay.dancher.DomArbitraries._
import com.github.unisay.dancher.interpreter.JsInterpreter.JsInterpreterElement
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{MustMatchers, PropSpec}
import dom._

class DomElementSpec extends PropSpec with GeneratorDrivenPropertyChecks with MustMatchers {

  property("domElement.getClass returns proper action when element has \"class\" attribute") {
    forAll { (element: JsInterpreterElement, classAttributeValue: String) =>
      val (result, script) = cssClass(element)
        .interpretJs(model = (), attributes = Map(element -> Map("class" -> classAttributeValue)))

      result mustBe Some(classAttributeValue)
      script mustBe s"""${element.name}.getAttribute("class");"""
    }
  }

  property("domElement.getClass returns proper action when element has no \"class\" attribute") {
    forAll { (element: JsInterpreterElement) =>
      val (result, script) = cssClass(element).interpretJs(model = ())

      result mustBe None
      script mustBe s"""${element.name}.getAttribute("class");"""
    }
  }

  property("domElement.getClass returns proper action when element has empty \"class\" attribute") {
    forAll { (element: JsInterpreterElement) =>
      val (result, script) = cssClass(element)
        .interpretJs(model = (), attributes = Map(element -> Map("class" -> "")))

      result mustBe Some("")
      script mustBe s"""${element.name}.getAttribute("class");"""
    }
  }

  property("domElement.setClass returns proper action for new class value") {
    forAll { (element: JsInterpreterElement, cssClass0: CssClass, cssClass1: CssClass) =>
      whenever(cssClass0 =!= cssClass1) {
        val (result, script) = addClass(cssClass1.value)(element)
          .interpretJs(model = (), attributes = Map(element -> Map("class" -> cssClass0.value)))

        result mustBe element
        script mustBe
          s"""
             |${element.name}.getAttribute("class");
             |${element.name}.setAttribute("class", "$cssClass0 $cssClass1");
          """.stripMargin.trim
      }
    }
  }

  property("domElement.setClass returns proper action for existing class value") {
    forAll { (element: JsInterpreterElement, cssClass: CssClass) =>
      val (result, script) = addClass(cssClass.value)(element)
        .interpretJs(model = (), attributes = Map(element -> Map("class" -> cssClass.value)))

      result mustBe element
      script mustBe
        s"""
           |${element.name}.getAttribute("class");
           |${element.name}.setAttribute("class", "$cssClass");
      """.stripMargin.trim
    }
  }

  property("domElement.removeClass returns proper action for existing class value") {
    forAll { (element: JsInterpreterElement, cssClass: CssClass) =>
      val (result, script) = removeClass(cssClass.value)(element)
        .interpretJs(model = (), attributes = Map(element -> Map("class" -> s"test-class $cssClass")))

      result mustBe element
      script mustBe
        s"""
           |${element.name}.getAttribute("class");
           |${element.name}.setAttribute("class", "test-class");
      """.stripMargin.trim
    }
  }

  property("domElement.removeClass returns proper action for non-existing class value") {
    forAll { (element: JsInterpreterElement, cssClass: CssClass) =>
      val (result, script) = removeClass(cssClass.value)(element)
        .interpretJs(model = ())

      result mustBe element
      script mustBe s"""${element.name}.getAttribute("class");""".trim
    }
  }

  property("domElement.setClasses returns proper action") {
    forAll { (element: JsInterpreterElement, cssClasses: NonEmptyList[CssClass]) =>
      val classes = cssClasses.map(_.value)
      val (result, script) = addClasses(element, classes)
        .interpretJs(model = (), attributes = Map(element -> Map("class" -> s"test-class ${classes.head}")))

      result mustBe element
      script mustBe s"""
         |${element.name}.getAttribute("class");
         |${element.name}.setAttribute("class", "${("test-class" :: classes.toList).sorted.mkString(" ")}");
      """.stripMargin.trim
    }
  }
}
