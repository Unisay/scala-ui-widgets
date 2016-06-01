package com.github.unisay.dancher

import dom._

trait Model {
  def get(id: DomId): Model
  def at(id: DomId)(f: Model ⇒ Model): Model
  def remove(id: DomId): Model
  def modify[W](id: DomId)(f: W ⇒ (W, ActionF[_])): Model

  def vertical(f: Model ⇒ Model)(implicit idGen: Gen[DomId]): Model = vertical(idGen.generate)(f)
  def vertical(id: DomId)(f: Model ⇒ Model): Model

  def horizontal(f: Model ⇒ Model): Model
  def horizontal(id: String)(f: Model ⇒ Model): Model
  def button(label: String, onClick: DomEventHandler = NoEventHandler): Model

  def label(text: String)(implicit idGen: Gen[DomId]): Model = label(idGen.generate, text)
  def label(id: Symbol, text: String): Model = label(DomId(id.name), text)
  def label(id: DomId, text: String): Model

  def action: ActionF[DomElement]
}

object ModelBuilder {
  def apply(): ModelBuilder = new ModelBuilder(Body(Gen[DomId].generate))
}

case class ModelBuilder(widget: Widget) extends Model {
  def get(id: DomId): Model = ???
  def at(id: DomId)(f: (Model) ⇒ Model): Model = ???
  def remove(id: DomId): Model = ???

  def button(label: String, onClick: DomEventHandler = NoEventHandler): Model = ???
  def horizontal(l: (Model) ⇒ Model): Model = ???
  def horizontal(id: String)(l: (Model) ⇒ Model): Model = ???


  def vertical(id: DomId)(f: (Model) ⇒ Model): Model = f(this)

  def label(id: DomId, text: String): Model =
    copy(widget = Label(id, text))

  def action: ActionF[DomElement] = ???
  def modify[W](id: DomId)(f: (W) ⇒ (W, ActionF[_])): Model = ???
}
