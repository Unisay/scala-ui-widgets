package com.github.unisay.dancher

import dom._

trait Model {
  def get(symbol: Symbol): Model
  def at(symbol: Symbol)(f: Model ⇒ Model): Model
  def remove(symbol: Symbol): Model
  def modify[W](symbol: Symbol)(f: W ⇒ (W, ActionF[_])): Model

  def vertical(f: Model ⇒ Model): Model
  def vertical(sym: Symbol)(f: Model ⇒ Model): Model
  def horizontal(f: Model ⇒ Model): Model
  def horizontal(sym: Symbol)(f: Model ⇒ Model): Model
  def button(label: String, onClick: DomEventHandler = NoEventHandler): Model
  def label(id: Symbol, text: String): Model
  def label(text: String): Model
  def apply(): ActionF[DomElement]
}

case class ModelBuilder() extends Model {
  def get(symbol: Symbol): Model = ???
  def at(symbol: Symbol)(f: (Model) ⇒ Model): Model = ???
  def remove(symbol: Symbol): Model = ???
  def modify[W](symbol: Symbol)(f: (W) ⇒ (W, ActionF[_])): Model = ???

  def button(label: String, onClick: DomEventHandler = NoEventHandler): Model = ???
  def horizontal(l: (Model) ⇒ Model): Model = ???
  def horizontal(sym: Symbol)(l: (Model) ⇒ Model): Model = ???
  def apply(): ActionF[DomElement] = ???
  def label(id: Symbol, text: String): Model = ???
  def label(text: String): Model = ???


  def vertical(f: Model ⇒ Model): Model = vertical(None, f)
  def vertical(sym: Symbol)(f: Model ⇒ Model): Model = vertical(Some(sym), f)

  private def vertical(symbol: Option[Symbol], f: Model ⇒ Model): Model = new ModelBuilder {
  }
}
