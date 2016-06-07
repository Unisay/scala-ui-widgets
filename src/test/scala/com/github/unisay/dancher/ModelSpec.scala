package com.github.unisay.dancher

import com.github.unisay.dancher.compiler.JsCompiler
import com.github.unisay.dancher.dom._
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class ModelSpec extends Specification with ScalaCheck {

  val genDomId: Gen[DomId] =
    Gen.alphaStr.map(s ⇒ DomId(s"id$s"))
  val genButton: Gen[Button] =
    for {domId ← genDomId; label ← Gen.alphaStr} yield Button(domId, label)
  val genLabel: Gen[Label] =
    for {domId ← genDomId; text ← Gen.alphaStr} yield Label(domId, text)
  def genVerticalLayout(n: Int) =
    for {domId ← genDomId; widgets ← genWidgets(n / 2)} yield VerticalLayout(domId, widgets)
  def genHorizontalLayout(n: Int) =
    for {domId ← genDomId; widgets ← genWidgets(n / 2)} yield HorizontalLayout(domId, widgets)
  def genWidget(n: Int): Gen[Widget] =
    Gen.oneOf(genVerticalLayout(n), genHorizontalLayout(n), genLabel, genButton)
  def genWidgets(n: Int): Gen[Seq[Widget]] =
    Gen.listOfN(n, genWidget(n / 2))

  implicit val ArbitraryLabel: Arbitrary[Label] = Arbitrary(genLabel)
  implicit val ArbitraryButton: Arbitrary[Button] = Arbitrary(genButton)
  implicit val ArbitraryModel: Arbitrary[Model] = Arbitrary(Gen.sized(n ⇒ genWidgets(n)).map(Model))

  "Model must" in {

    "create label widget" in prop { (model: Model, label: Label) ⇒
      model.label(label.domId, label.text).widgets must contain(label)
    }

    "create label action" in prop { (model: Model, label: Label) ⇒
      val action: ActionF[DomElement] = model.label(label.domId, label.text).actions.head
      val written = new JsCompiler().compile(action).written
      written must_===
        "var span1 = document.createElement('span')" ::
        s"span1.setAttribute('id', '${label.domId.value}')" ::
        "span1.setAttribute('class', 'd-label')" ::
        s"var text2 = document.createTextNode('${label.text}')" ::
        "span1.appendChild(text2)" :: Nil
    }

    "create button widget" in prop { (model: Model, button: Button) ⇒
      model.button(button.domId, button.label, button.clickHandler).widgets must contain(button)
    }

    "create button action" in prop { (model: Model, button: Button) ⇒
      model.button(button.domId, button.label, button.clickHandler).actions must contain(button.create)
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

  }

}
