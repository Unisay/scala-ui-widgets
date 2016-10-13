package com.github.unisay.dancher.widget

import com.github.unisay.dancher.Arbitraries._
import com.github.unisay.dancher.TestUtils._
import com.github.unisay.dancher.Widget._
import com.github.unisay.dancher.widget.BasicWidgets.body
import com.github.unisay.dancher.widget.LayoutWidgets._
import org.scalajs.dom.html.Div
import org.scalatest._

class LayoutWidgetsSpec extends AsyncFlatSpec with MustMatchers with Inspectors {

  implicit override def executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
  implicit val doc = org.scalajs.dom.document

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

  behavior of "HorizontalSplit.div.edge"

  it must "be rendered" in {
    renderHorizontalSplit assert (_.element.children.item(1).tagName mustBe "DIV")
  }

  it must "have css classes" in {
    renderHorizontalSplit assert {
      _.element.children.item(1).asInstanceOf[Div].className mustEqual "d-split-horizontal-edge"
    }
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

  behavior of "HorizontalSplit widget"

  it must "move edge" in {
    renderHorizontalSplit assert { binding =>
      val mainDiv = binding.element.asInstanceOf[Div]
      val leftDiv = mainDiv.children.item(0).asInstanceOf[Div]
      val edgeDiv = mainDiv.children.item(1).asInstanceOf[Div]

      val mainRect = mainDiv.getBoundingClientRect()
      val leftRect = leftDiv.getBoundingClientRect()
      val edgeRect = edgeDiv.getBoundingClientRect()

      println(edgeRect.height)
      println(edgeRect.width)
      println(edgeRect.left)
      println(edgeRect.top)

      mouseMove(leftDiv, x = edgeRect.left - 2, y = mainRect.top + 2)
      mouseMove(edgeDiv, x = edgeRect.left + 1, y = mainRect.top + 2)

      succeed
    }
  }

  private def renderHorizontalSplit = {
    val (leftWidget, _) = createWidget("p")
    val (rightWidget, _) = createWidget("a")
    body(horizontalSplit(leftWidget, rightWidget))
      .render
      .map(_.nested.head)
  }

}
