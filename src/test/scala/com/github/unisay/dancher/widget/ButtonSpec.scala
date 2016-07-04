package com.github.unisay.dancher.widget

import com.github.unisay.dancher.Arbitraries._
import com.github.unisay.dancher.Matchers._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class ButtonSpec extends Specification with ScalaCheck {

  "Button" should {

    "create" in prop { (button: Button) ⇒
      button.create.asScript must containScriptTemplate(s"""
        var button\\d+ = document\\.createElement\\('button'\\)
        button\\d+\\.setAttribute\\('id', '${button.domId.value}'\\)
        button\\d+\\.setAttribute\\('class', 'd-button'\\)
        var text\\d+ = document\\.createTextNode\\('${button.label}'\\)
        button\\d+\\.appendChild\\(text\\d+\\)
        /\\* SetOnClick\\(button\\d+\\) \\*/
      """)
    }

  }
}
