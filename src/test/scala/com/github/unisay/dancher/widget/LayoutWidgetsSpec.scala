package com.github.unisay.dancher.widget

import com.github.unisay.dancher.Arbitraries._
import com.github.unisay.dancher.Binding
import com.github.unisay.dancher.TestUtils._
import com.github.unisay.dancher.Widget._
import com.github.unisay.dancher.widget.LayoutWidgets._
import org.scalajs.dom.raw.HTMLDivElement
import org.scalatest._

class LayoutWidgetsSpec extends AsyncFlatSpec with MustMatchers with Inspectors {

  implicit override def executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  behavior of "HorizontalSplit"

  it must "render dom elements" in {
    val (leftWidget, leftBinding) = createWidget(1)
    val (rightWidget, rightBinding) = createWidget(2)

    horizontalSplit(leftWidget, rightWidget).render.assert { (binding: Binding) =>
      binding.element mustBe a[HTMLDivElement]
      binding.element.childElementCount mustBe 3
      val children = binding.element.children

      withClue ("Left Div") {
        children.item(0) must not be null
        children.item(0) mustBe a[HTMLDivElement]
        val leftDiv = children.item(0).asInstanceOf[HTMLDivElement]
        forAll(List("d-split-horizontal", "d-split-horizontal-left", "d-no-select", "d-no-drag")) {
          _ must be((c: String) => leftDiv.classList.contains(c))
        }
      }

      children.item(1) mustBe a[HTMLDivElement]
      val edgeDiv = children.item(1).asInstanceOf[HTMLDivElement]
      forAll(List("d-split-horizontal", "d-split-horizontal-left", "d-no-select", "d-no-drag")) {
        _ must be((c: String) => edgeDiv.classList.contains(c))
      }

      children.item(2) mustBe a[HTMLDivElement]
      val rightDiv = children.item(2).asInstanceOf[HTMLDivElement]
      forAll(List("d-split-horizontal", "d-split-horizontal-left", "d-no-select", "d-no-drag")) {
        _ must be((c: String) => rightDiv.classList.contains(c))
      }

      succeed
    }
  }

}
