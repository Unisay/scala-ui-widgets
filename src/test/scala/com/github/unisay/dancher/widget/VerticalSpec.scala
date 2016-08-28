package com.github.unisay.dancher.widget

import com.github.unisay.dancher.ActionTestHelpers._
import com.github.unisay.dancher.dom.DomBinding
import com.github.unisay.dancher.interpreter.JsInterpreter
import com.github.unisay.dancher.interpreter.JsInterpreter.JsInterpreterElement
import com.github.unisay.dancher.widget.Widget._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{MustMatchers, PropSpec}

class VerticalSpec extends PropSpec with GeneratorDrivenPropertyChecks with MustMatchers {

  implicit val interpreter = JsInterpreter
  import interpreter._

  val model = ()
  val vertical: Widget[Unit] = Vertical(
    Label[Unit](const("Label1")) >
    Label[Unit](const("Label2"))
  )

  property("Vertical ") {
    forAll { (s: String) =>
      val renderAction = vertical(model)
      val (element, nested, events, script) = renderAction.interpretJs(model = ())

      element mustBe JsInterpreterElement("div0")
      nested must contain theSameElementsInOrderAs List(
        DomBinding(JsInterpreterElement("span0")),
        DomBinding(JsInterpreterElement("span1"))
      )
      script mustBe
        s"""
           |var div0 = document.createElement('div');
           |div0.setAttribute("class", "d-vertical");
           |var span0 = document.createElement('span');
           |var text0 = document.createTextNode('Label1');
           |span0.appendChild(text0);
           |span0.setAttribute("class", "d-label");
           |div0.appendChild(span0);
           |var span1 = document.createElement('span');
           |var text1 = document.createTextNode('Label2');
           |span1.appendChild(text1);
           |span1.setAttribute("class", "d-label");
           |div0.appendChild(span1);
         """.stripMargin.trim
    }
  }

}
