package com.github.unisay.dancher

import com.github.unisay.dancher.dom.DomId
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

  implicit val ArbitraryDomId: Arbitrary[DomId] = Arbitrary(genDomId)
  implicit val ArbitraryLabel: Arbitrary[Label] = Arbitrary(genLabel)
  implicit val ArbitraryButton: Arbitrary[Button] = Arbitrary(genButton)
  implicit val ArbitraryModel: Arbitrary[Model] = Arbitrary {
    for {
      domId ← genDomId
      label ← Gen.alphaStr
      gm ← Gen.oneOf(Gen.const(Model()), Arbitrary.arbitrary[Model])
      model ← Gen.oneOf(
        Model(),
        Model().label(domId, label),
        Model().horizontal(_ ⇒ gm),
        Model().vertical(_ ⇒ gm))
    } yield model
  }

}
