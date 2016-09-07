package com.github.unisay.dancher.widget

import com.github.unisay.dancher.ActionTestHelpers._
import com.github.unisay.dancher.widget.Widget._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{MustMatchers, PropSpec}

class VerticalSpec extends PropSpec with GeneratorDrivenPropertyChecks with MustMatchers {

  val model = ()
  val vertical: Widget[Unit] = Vertical(
    Label(const("Label1")) >
    Label(const("Label2"))
  )

  property("Vertical ") {
    forAll { (s: String) =>
      val renderAction = vertical(model)
      val (element, nested, events, script) = renderAction.interpretJs(model = ())

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
