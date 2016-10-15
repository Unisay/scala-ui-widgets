package com.github.unisay.dancher.widget

import com.github.unisay.dancher.Arbitraries._
import com.github.unisay.dancher.Binding
import com.github.unisay.dancher.TestUtils._
import com.github.unisay.dancher.Widget._
import com.github.unisay.dancher.widget.BasicWidgets.body
import com.github.unisay.dancher.widget.LayoutWidgets._
import org.scalajs.dom.Element
import org.scalajs.dom.html.Div
import org.scalatest._

class LayoutWidgetsSpec extends AsyncFlatSpec with MustMatchers with Inspectors {

  implicit override def executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
  implicit val doc = org.scalajs.dom.document

  behavior of "VerticalSplit.div"

  it must "be rendered" in {
    renderVerticalSplit assert (_.element.tagName mustBe "DIV")
  }

  it must "contain 3 children elements" in {
    renderVerticalSplit assert (_.element.childElementCount mustBe 3)
  }

  behavior of "VerticalSplit.div.left"

  it must "be rendered" in {
    renderVerticalSplit assert (_.element.children.item(0).tagName mustBe "DIV")
  }

  it must "have css classes" in {
    renderVerticalSplit assert { binding =>
      val classes = binding.element.children.item(0).asInstanceOf[Div].className.split(' ')
      classes must contain allOf("d-split-vertical-side", "d-split-vertical-side-left", "d-no-select", "d-no-drag")
    }
  }

  it must "contain root element of child widget" in {
    renderVerticalSplit assert (_.element.children.item(0).firstElementChild.tagName mustBe "P")
  }

  behavior of "VerticalSplit.div.edge"

  it must "be rendered" in {
    renderVerticalSplit assert (_.element.children.item(1).tagName mustBe "DIV")
  }

  it must "have css classes" in {
    renderVerticalSplit assert {
      _.element.children.item(1).asInstanceOf[Div].className mustEqual "d-split-vertical-edge"
    }
  }

  behavior of "VerticalSplit.div.right"

  it must "be rendered" in {
    renderVerticalSplit assert (_.element.children.item(2).tagName mustBe "DIV")
  }

  it must "have css classes" in {
    renderVerticalSplit assert { binding =>
      val classes = binding.element.children.item(2).asInstanceOf[Div].className.split(' ')
      classes must contain allOf("d-split-vertical-side", "d-split-vertical-side-right", "d-no-select", "d-no-drag")
    }
  }

  it must "contain root element of child widget" in {
    renderVerticalSplit assert (_.element.children.item(2).firstElementChild.tagName mustBe "A")
  }

  behavior of "VerticalSplit widget"

  it must "move edge" in {
    renderVerticalSplit unsafeRunAsyncFuture() flatMap { (binding: Binding) =>
      val mainDiv = binding.element.asInstanceOf[Div]
      val leftDiv = mainDiv.children.item(0).asInstanceOf[Div]
      val edgeDiv = mainDiv.children.item(1).asInstanceOf[Div]

      val mainRect = mainDiv.getBoundingClientRect()
      val leftRect = leftDiv.getBoundingClientRect()
      val edgeRect = edgeDiv.getBoundingClientRect()

      val x = edgeRect.left
      val y = mainRect.top + 2

      mouseMove (leftDiv, x - 5, y)
      mouseMove (edgeDiv, x + 1, y)
      mouseDown (edgeDiv, x + 1, y)
      mouseMove (leftDiv, x - 5, y)
      mouseUp   (leftDiv, x - 5, y)

      binding.deepDomEvents.take(0).run.unsafeRunAsyncFuture().map { _ =>
        edgeDiv.getBoundingClientRect().left mustBe (x - 5)
      }
    }
  }

  private def renderVerticalSplit = {
    val (leftWidget, _) = createWidget("p")
    val (rightWidget, _) = createWidget("a")
    body(
      verticalSplit(
        leftWidget.useElement{ (e: Element) => e.appendChild(doc.createTextNode("text")); () },
        rightWidget.useElement{ (e: Element) => e.appendChild(doc.createTextNode("link")); () }
      )
    )
    .render
    .map(_.nested.head)
  }

}
