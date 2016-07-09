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
      val model = builder.label(label.domId, label.text).build(body).model
      model.widgetContainer.children must contain(label)
      model.paths must haveKey(label.domId)
    }

    "create label action" in prop { (builder: ModelBuilder, body: Body, label: Label) ⇒
      val action = builder.label(label.domId, label.text).build(body).action
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
      val model = builder.button(button.domId, button.label).build(body).model
      model.widgetContainer.children must contain(button)
      model.paths must haveKey(button.domId)
    }

    "create button action" in prop { (builder: ModelBuilder, body: Body, button: Button) ⇒
      val action = builder.button(button.domId, button.label).build(body).action
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
      val model = builder.vertical(id)(_.label(label.domId, label.text)).build(body).model
      model.widgetContainer.children must contain(VerticalLayout(id, Vector(label)))
      model.paths must haveKey(id)
      model.paths must haveKey(label.domId)
    }

    "vertical layout action" in prop { (builder: ModelBuilder, body: Body, label: Label, id: DomId) ⇒
      val action = builder.vertical(id)(_.label(label.domId, label.text)).build(body).action
      action.asScript must containScriptTemplate("""
        var div\d+ = document\.createElement\('div'\)
        div\d+\.setAttribute\('id', 'id\w*'\)
        div\d+\.setAttribute\('class', 'd-vertical-layout'\)
        b\.appendChild\(div\d+\)
       """)
    }

    "horizontal layout model" in prop { (builder: ModelBuilder, body: Body, label: Label, id: DomId) ⇒
      val model = builder.horizontal(id)(_.label(label.domId, label.text)).build(body).model
      model.widgetContainer.children must contain(HorizontalLayout(id, Vector(label)))
      model.paths must haveKey(id)
      model.paths must haveKey(label.domId)
    }

    "horizontal layout action" in prop { (builder: ModelBuilder, body: Body, label: Label, id: DomId) ⇒
      val action = builder.horizontal(id)(_.label(label.domId, label.text)).build(body).action
      action.asScript must containScriptTemplate("""
        var div\d+ = document\.createElement\('div'\)
        div\d+\.setAttribute\('id', 'id\w*'\)
        div\d+\.setAttribute\('class', 'd-horizontal-layout'\)
        b\.appendChild\(div\d+\)
       """)
    }

    "vertical, horizontal" in prop { (builder: ModelBuilder, body: Body, v: DomId, h: DomId) ⇒
      (v != h) ==> {
        val model = builder
          .vertical(v)(identity)
          .horizontal(h)(identity)
          .build(body)
          .model

        model.widgetContainer.children must contain(VerticalLayout(v), HorizontalLayout(h))
        model.paths must haveKey(v)
        model.paths must haveKey(h)
      }
    }

    "get nested label" in prop { (builder: ModelBuilder, body: Body, label: Label) ⇒
      val model = builder
        .horizontal {
          _.vertical {
            _.horizontal {
              _.vertical {
                _.label(label.domId, label.text)
              }
            }
          }
        }
        .build(body)
        .model

      model.get(label.domId) must beSome(label)
    }

    "modify nested label" in prop { (builder: ModelBuilder, body: Body, label: Label) ⇒
      val model = builder
        .horizontal {
          _.vertical {
            _.horizontal {
              _.vertical {
                _.label(label.domId, label.text)
              }
            }
          }
        }
        .build(body)
        .model

      val Some(Frame(modifiedModel, _)) = model.modify[Label](label.domId)(_.setText("modified"))
      val expectedLabel = label.setText("modified")._1
      val maybeActualLabel = modifiedModel.get(label.domId)
      maybeActualLabel must beSome(expectedLabel)
    }

  }

}
