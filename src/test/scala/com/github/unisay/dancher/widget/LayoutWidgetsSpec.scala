package com.github.unisay.dancher.widget

import com.github.unisay.dancher.Arbitraries._
import com.github.unisay.dancher.TestUtils._
import com.github.unisay.dancher.Widget._
import com.github.unisay.dancher._
import com.github.unisay.dancher.widget.BasicWidgets.body
import com.github.unisay.dancher.widget.LayoutWidgets._
import fs2.Scheduler
import org.scalajs.dom.Element
import org.scalajs.dom.html.Div
import org.scalatest._


class LayoutWidgetsSpec extends AsyncFlatSpec with MustMatchers with Inspectors with BeforeAndAfterEach {

  implicit override def executionContext = scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
  implicit val doc = org.scalajs.dom.document
  implicit val scheduler = Scheduler.default

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
    renderVerticalSplit assert { binding =>
      val classes = binding.element.children.item(1).asInstanceOf[Div].className.split(' ')
      classes must contain allOf ("d-split-vertical-edge", "d-split-vertical-resize")
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
    (renderVerticalSplit flatMap { binding =>
      val mainDiv = binding.element.asInstanceOf[Div]
      val leftDiv = mainDiv.children.item(0).asInstanceOf[Div]
      val edgeDiv = mainDiv.children.item(1).asInstanceOf[Div]

      val x = edgeDiv.offsetLeft
      val y = mainDiv.offsetTop + 5

      mouseMove(mainDiv, x - 5, y)
      mouseMove(mainDiv, x + 1, y)
      mouseDown(mainDiv, x + 1, y)
      mouseMove(mainDiv, x - 5, y)
      mouseUp(mainDiv, x - 5, y)

      binding.domainEvents.take(1).runLog.map(binding -> _)
    })
    .assert { case (binding, log) => log must contain(SplitResized(binding, 483)) }
  }

  override protected def beforeEach() = doc.body.removeAllChildren()

  private def renderVerticalSplit: Widget = {
    val (leftWidget, _) = createWidget("p")
    val (rightWidget, _) = createWidget("a")
    body(
      verticalSplit(
        leftWidget.useElement { (e: Element) => e.appendChild(doc.createTextNode("text")); () },
        rightWidget.useElement { (e: Element) => e.appendChild(doc.createTextNode("link")); () }
      ).identifiedBy('vs1)
    )
    .map(_.nested.head)
  }

}
