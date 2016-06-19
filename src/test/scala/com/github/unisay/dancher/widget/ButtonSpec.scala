package com.github.unisay.dancher.widget

import com.github.unisay.dancher.Arbitraries._
import com.github.unisay.dancher.Matchers._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class ButtonSpec extends Specification with ScalaCheck {

  "Button" should {

    "create" in prop { (button: Button) ⇒
      button.create must beActionAsScript(
        s"""
        |var button1 = document.createElement('button')
        |button1.setAttribute('id', '${button.domId.value}')
        |button1.setAttribute('class', 'd-button')
        |var text2 = document.createTextNode('${button.label}')
        |button1.appendChild(text2)
        |/* SetOnClick(button1) */
        """
      )
    }

  }
}
