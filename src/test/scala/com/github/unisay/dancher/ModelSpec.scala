package com.github.unisay.dancher

import com.github.unisay.dancher.Arbitraries._
import com.github.unisay.dancher.Matchers._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class ModelSpec extends Specification with ScalaCheck {

  "Model must" in {

    "create label widget" in prop { (model: Model, label: Label) ⇒
      model.label(label.domId, label.text).widgets must contain(label)
    }

    "create label action" in prop { (model: Model, label: Label) ⇒
      model.label(label.domId, label.text).actions must contain(expectedAction(label.create))
    }

    "create button widget" in prop { (model: Model, button: Button) ⇒
      model.button(button.domId, button.label, button.clickHandler).widgets must contain(button)
    }

    "create button action" in prop { (model: Model, button: Button) ⇒
      val actions = model.button(button.domId, button.label, button.clickHandler).actions
      actions must contain(expectedAction(button.create))
    }

    "vertical layout" in prop { (model: Model) ⇒
      model.vertical('id) {
        _.label('id2, "label")
      }.widgets must contain(
        VerticalLayout('id, Seq(Label('id2, "label")))
      )
    }

    "horizontal layout" in prop { (model: Model) ⇒
      model.horizontal('id) {
        _.label('id2, "label")
      }.widgets must contain(
        HorizontalLayout('id, Seq(Label('id2, "label")))
      )
    }

    "vertical, horizontal" in prop { (model: Model) ⇒
      model.vertical('v)(identity).horizontal('h)(identity).widgets must contain(
        VerticalLayout('v, Nil),
        HorizontalLayout('h, Nil)
      )
    }

    "get" in prop { (model: Model, label: Label) ⇒
      label must_== model.label(label.domId, label.text).get(label.domId)
    }
  }

}
