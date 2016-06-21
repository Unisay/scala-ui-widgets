package com.github.unisay.dancher

import com.github.unisay.dancher.dom.DomId
import com.github.unisay.dancher.widget._
import org.scalacheck.{Arbitrary, Gen}

object Arbitraries {

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
  def genWidgets(n: Int): Gen[Vector[Widget]] =
    Gen.listOfN(n, genWidget(n / 2)).map(_.toVector)

  implicit val arbDomId: Arbitrary[DomId] = Arbitrary(genDomId)
  implicit val arbLabel: Arbitrary[Label] = Arbitrary(genLabel)
  implicit val arbButton: Arbitrary[Button] = Arbitrary(genButton)

  implicit val arbHorizontalLayout: Arbitrary[HorizontalLayout] = Arbitrary {
    for { n ← Gen.size; domId ← genDomId; children ← genWidgets(n) } yield HorizontalLayout(domId, children)
  }

  implicit val arbVerticalLayout: Arbitrary[VerticalLayout] = Arbitrary {
    for { n ← Gen.size; domId ← genDomId; children ← genWidgets(n) } yield VerticalLayout(domId, children)
  }

  implicit val arbModel: Arbitrary[ModelBuilder] = Arbitrary {
    for {
      domId ← genDomId
      label ← Gen.alphaStr
      gm ← Gen.oneOf(Gen.const(ModelBuilder()), Arbitrary.arbitrary[ModelBuilder])
      model ← Gen.oneOf(
        ModelBuilder(),
        ModelBuilder().label(domId, label),
        ModelBuilder().horizontal(_ ⇒ gm),
        ModelBuilder().vertical(_ ⇒ gm))
    } yield model
  }

}
