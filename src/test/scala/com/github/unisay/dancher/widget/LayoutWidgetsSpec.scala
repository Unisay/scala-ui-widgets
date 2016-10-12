package com.github.unisay.dancher.widget

import com.github.unisay.dancher.Arbitraries._
import com.github.unisay.dancher.TestUtils._
import com.github.unisay.dancher.Widget._
import com.github.unisay.dancher.widget.LayoutWidgets._
import org.scalajs.dom.html.Div
import org.scalatest._

class LayoutWidgetsSpec extends AsyncFlatSpec with MustMatchers with Inspectors {

  implicit override def executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  behavior of "HorizontalSplit.div"

  it must "be rendered" in {
    renderHorizontalSplit assert (_.element.tagName mustBe "DIV")
  }

  it must "contain 3 children elements" in {
    renderHorizontalSplit assert (_.element.childElementCount mustBe 3)
  }

  behavior of "HorizontalSplit.div.left"

  it must "be rendered" in {
    renderHorizontalSplit assert (_.element.children.item(0).tagName mustBe "DIV")
  }

  it must "have css classes" in {
    renderHorizontalSplit assert { binding =>
      val classes = binding.element.children.item(0).asInstanceOf[Div].className.split(' ')
      classes must contain allOf("d-split-horizontal-side", "d-split-horizontal-side-left", "d-no-select", "d-no-drag")
    }
  }

  it must "contain root element of child widget" in {
    renderHorizontalSplit assert (_.element.children.item(0).firstElementChild.tagName mustBe "P")
  }

  behavior of "HorizontalSplit.div.right"

  it must "be rendered" in {
    renderHorizontalSplit assert (_.element.children.item(2).tagName mustBe "DIV")
  }

  it must "have css classes" in {
    renderHorizontalSplit assert { binding =>
      val classes = binding.element.children.item(2).asInstanceOf[Div].className.split(' ')
      classes must contain allOf("d-split-horizontal-side", "d-split-horizontal-side-right", "d-no-select", "d-no-drag")
    }
  }

  it must "contain root element of child widget" in {
    renderHorizontalSplit assert (_.element.children.item(2).firstElementChild.tagName mustBe "A")
  }

  private def renderHorizontalSplit = {
    val (leftWidget, _) = createWidget("p")
    val (rightWidget, _) = createWidget("a")
    horizontalSplit(leftWidget, rightWidget).render
  }

}
