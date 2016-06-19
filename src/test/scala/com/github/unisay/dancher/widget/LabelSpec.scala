package com.github.unisay.dancher.widget

import com.github.unisay.dancher.Arbitraries._
import com.github.unisay.dancher.Matchers._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class LabelSpec extends Specification with ScalaCheck {

  "Label" should {

    "create" in prop { (label: Label) ⇒
      label.create must beActionAsScript {
        s"""
        |var span1 = document.createElement('span')
        |span1.setAttribute('id', '${label.domId.value}')
        |span1.setAttribute('class', 'd-label')
        |var text2 = document.createTextNode('${label.text}')
        |span1.appendChild(text2)
        """
      }
    }

    "setText" in prop { (label: Label, text: String) ⇒
      (label.text != text) ==> {
        val (modifiedLabel, action) = label.setText(text)
        modifiedLabel.text mustEqual text
        action must beActionAsScript {
          s"""
          |var element1 = document.getElementById('${label.domId.value}')
          |var text2 = document.createTextNode('$text')
          |element1.replaceChild(text2, element1.firstChild)
          """
        }
      }
    }

  }
}
