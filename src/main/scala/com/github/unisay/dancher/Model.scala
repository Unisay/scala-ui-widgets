package com.github.unisay.dancher

import dom._

case class Model(widgets: Seq[Widget] = Seq.empty, actions: Seq[WidgetAction] = Seq.empty) {

  def get(id: DomId): Model = ???
  def at(id: DomId)(f: (Model) ⇒ Model): Model = ???
  def remove(id: DomId): Model = ???
  def modify[W](id: DomId)(f: W ⇒ (W, ActionF[_])): Model = ???

  def vertical(f: Model ⇒ Model)(implicit idGen: Generator[DomId]): Model =
    vertical(idGen.generate)(f)
  def vertical(id: DomId)(f: Model ⇒ Model): Model =
    appendWidget(VerticalLayout(id, f(Model(Seq.empty)).widgets))

  def horizontal(f: Model ⇒ Model)(implicit idGen: Generator[DomId]): Model =
    horizontal(idGen.generate)(f)
  def horizontal(id: DomId)(f: Model ⇒ Model): Model =
    appendWidget(HorizontalLayout(id, f(Model(Seq.empty)).widgets))

  def button(label: String, onClick: DomEventHandler)(implicit idGen: Generator[DomId]): Model =
    button(idGen.generate, label, onClick)
  def button(id: Symbol, label: String, clickHandler: DomEventHandler): Model =
    button(DomId(id.name), label, clickHandler)
  def button(id: DomId, label: String, clickHandler: DomEventHandler): Model =
    appendWidget(Button(id, label, clickHandler))

  def label(text: String)(implicit idGen: Generator[DomId]): Model =
    label(idGen.generate, text)
  def label(id: Symbol, text: String): Model =
    label(DomId(id.name), text)
  def label(id: DomId, text: String): Model =
    appendWidget(Label(id, text))

  private def appendWidget(widget: Widget): Model = this.copy(
    widgets = this.widgets :+ widget,
    actions = List(widget.create)
  )
}

object BodyModel {
  def apply(): Model = Model(Seq(Body(Generator[DomId].generate)))
}
