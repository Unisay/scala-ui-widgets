package com.github.unisay.dancher

import com.github.unisay.dancher.Arbitraries._
import com.github.unisay.dancher.Matchers._
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.widget._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class ModelSpec extends Specification with ScalaCheck {

  "Model must" in {

    "create label widget" in prop { (model: ModelBuilder, label: Label) ⇒
      model.label(label.domId, label.text).widgets must contain(label)
    }

    "create label action" in prop { (model: ModelBuilder, label: Label) ⇒
      model.label(label.domId, label.text).actions must contain(expectedAction(label.create))
    }

    "create button widget" in prop { (model: ModelBuilder, button: Button) ⇒
      model.button(button.domId, button.label).widgets must contain(button)
    }

    "create button action" in prop { (model: ModelBuilder, button: Button) ⇒
      val actions = model.button(button.domId, button.label).actions
      actions must contain(expectedAction(button.create))
    }

    "vertical layout" in prop { (model: ModelBuilder, label: Label, id: DomId) ⇒
      model.vertical(id)(_.label(label.domId, label.text)).widgets must contain(VerticalLayout(id, Vector(label)))
    }

    "horizontal layout" in prop { (model: ModelBuilder, label: Label, id: DomId) ⇒
      model.horizontal(id)(_.label(label.domId, label.text)).widgets must contain(HorizontalLayout(id, Vector(label)))
    }

    "vertical, horizontal" in prop { (model: ModelBuilder, v: DomId, h: DomId) ⇒
      (v != h) ==> {
        model.vertical(v)(identity).horizontal(h)(identity).widgets must
          contain(VerticalLayout(v, Vector.empty), HorizontalLayout(h, Vector.empty))
      }
    }

    "get nested label" in prop { (model: ModelBuilder, label: Label) ⇒
      model
        .horizontal {
          _.vertical {
            _.horizontal {
              _.vertical {
                _.label(label.domId, label.text)
              }
            }
          }
        }.get(label.domId) must beSome(label)
    }

    "modify nested label" in prop { (model: ModelBuilder, label: Label, button: Button) ⇒
      (button.domId != label.domId) ==> {
        val nestedModel = model
          .horizontal {
            _.vertical {
              _.horizontal {
                _.vertical {
                  _.label(label.domId, label.text)
                }
              }
            }
          }
          .button(button.domId, button.label)

        val modifiedModel = nestedModel.modify[Label](label.domId)(_.setText(label.text + "!"))
        val modifiedLabel = label.copy(text = label.text + "!")
        modifiedModel.get(label.domId) must beSome(modifiedLabel)
        modifiedModel.get(button.domId) must beSome(button)
      }
    }

  }

}
