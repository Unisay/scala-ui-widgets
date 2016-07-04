package com.github.unisay.dancher

import com.github.unisay.dancher.Arbitraries._
import com.github.unisay.dancher.Matchers._
import com.github.unisay.dancher.dom.DomId
import com.github.unisay.dancher.widget._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class ModelBuilderSpec extends Specification with ScalaCheck {

  "ModelBuilder must" in {

    "create label model" in prop { (builder: ModelBuilder, body: Body, label: Label) ⇒
      val (model, _) = builder.label(label.domId, label.text).build(body)
      model.widgetContainer.children must contain(label)
      model.paths must haveKey(label.domId)
    }

    "create label action" in prop { (builder: ModelBuilder, body: Body, label: Label) ⇒
      val (_, action) = builder.label(label.domId, label.text).build(body)
      action.asScript must containScriptTemplate("""
        var span\d+ = document\.createElement\('span'\)
        span\d+\.setAttribute\('id', 'id\w*'\)
        span\d+\.setAttribute\('class', 'd-label'\)
        var text\d+ = document\.createTextNode\('\w*'\)
        span\d+\.appendChild\(text\d+\)
        b\.appendChild\(span\d+\)
      """)
    }

    "create button model" in prop { (builder: ModelBuilder, body: Body, button: Button) ⇒
      val (model, _) = builder.button(button.domId, button.label).build(body)
      model.widgetContainer.children must contain(button)
      model.paths must haveKey(button.domId)
    }

    "create button action" in prop { (builder: ModelBuilder, body: Body, button: Button) ⇒
      val (_, action) = builder.button(button.domId, button.label).build(body)
      action.asScript must containScriptTemplate("""
        var button\d+ = document\.createElement\('button'\)
        button\d+\.setAttribute\('id', 'id\w*'\)
        button\d+\.setAttribute\('class', 'd-button'\)
        var text\d+ = document\.createTextNode\('\w*'\)
        button\d+\.appendChild\(text\d+\)
        b\.appendChild\(button\d+\)
      """)
    }

    "vertical layout model" in prop { (builder: ModelBuilder, body: Body, label: Label, id: DomId) ⇒
      val (model, _) = builder.vertical(id)(_.label(label.domId, label.text)).build(body)
      model.widgetContainer.children must contain(VerticalLayout(id, Vector(label)))
      model.paths must haveKey(id)
      model.paths must haveKey(label.domId)
    }

    "vertical layout action" in prop { (builder: ModelBuilder, body: Body, label: Label, id: DomId) ⇒
      val (_, action) = builder.vertical(id)(_.label(label.domId, label.text)).build(body)
      action.asScript must containScriptTemplate("""
        var div\d+ = document\.createElement\('div'\)
        div\d+\.setAttribute\('id', 'id\w*'\)
        div\d+\.setAttribute\('class', 'd-vertical-layout'\)
        b\.appendChild\(div\d+\)
       """)
    }

    "horizontal layout model" in prop { (builder: ModelBuilder, body: Body, label: Label, id: DomId) ⇒
      val (model, _) = builder.horizontal(id)(_.label(label.domId, label.text)).build(body)
      model.widgetContainer.children must contain(HorizontalLayout(id, Vector(label)))
      model.paths must haveKey(id)
      model.paths must haveKey(label.domId)
    }

    "horizontal layout action" in prop { (builder: ModelBuilder, body: Body, label: Label, id: DomId) ⇒
      val (_, action) = builder.horizontal(id)(_.label(label.domId, label.text)).build(body)
      action.asScript must containScriptTemplate("""
        var div\d+ = document\.createElement\('div'\)
        div\d+\.setAttribute\('id', 'id\w*'\)
        div\d+\.setAttribute\('class', 'd-horizontal-layout'\)
        b\.appendChild\(div\d+\)
       """)
    }

    /*

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
    }*/

  }

}
