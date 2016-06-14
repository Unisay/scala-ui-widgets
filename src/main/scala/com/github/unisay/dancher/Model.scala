package com.github.unisay.dancher

import com.github.unisay.dancher.Model.Path
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.widget.{Body, Button, HasChildren, Label, VerticalLayout}
import monocle.Optional
import monocle.function.Index._
import monocle.std.vector._

object Model {
  type Path[W] = Optional[Vector[W], W]
  def body = Model(widgets = Vector(Body(Generator[DomId].generate)))
}

case class Model[W : Widget](widgets: Vector[W] = Vector.empty,
                 actions: Vector[ActionF[_]] = Vector.empty,
                 paths: Map[DomId, Path[W]] = Map.empty) {
  import Model._

  def get(id: DomId): Option[W] = paths.get(id) flatMap (_.getOption(widgets))
  def getAs(id: DomId): Option[W] = get(id)

  def modify(id: DomId)(change: W ⇒ (W, ActionF[_])): Model[W] = {
    paths.get(id).flatMap { path ⇒
      path.getOption(widgets).map { widget ⇒
        val (modifiedWidget, action) = change(widget)
        copy(widgets = path.set(modifiedWidget).apply(widgets), actions = actions :+ action)
      }
    }.getOrElse(this)
  }

  def within(id: DomId)(f: Model[W] ⇒ Model[W]): Model[W] = ???

  def vertical(f: Model[W] ⇒ Model[W])(implicit idGen: Generator[DomId]): Model[W] =
    vertical(idGen.generate)(f)

  def vertical(id: DomId)(f: Model[W] ⇒ Model[W]): Model[W] = {
    val nestedModel = f(Model())
    val optionalCurrentWidget: Path[W] = index(widgets.length)
    val optionalChildren = optionalCurrentWidget.composeOptional(implicitly[HasChildren[VerticalLayout[W]]]._children)
    val childrenPaths = nestedModel.paths.mapValues(optionalChildren.composeOptional)
    val widget = VerticalLayout(id, nestedModel.widgets)
    copy(
      widgets = widgets :+ widget,
      actions = actions :+ widget.create,
      paths = paths + (id → optionalCurrentWidget) ++ childrenPaths
    )
  }

  def horizontal(f: Model[W] ⇒ Model[W])(implicit idGen: Generator[DomId]): Model[W] =
    horizontal(idGen.generate)(f)

  def horizontal(id: DomId)(f: Model[W] ⇒ Model[W]): Model[W] = {
    val nestedModel = f(Model())
    val optionalCurrentWidget: Path[W] = index(widgets.length)
    val optionalChildren = optionalCurrentWidget.composeOptional(HorizontalLayout._children)
    val childrenPaths = nestedModel.paths.mapValues(optionalChildren.composeOptional)
    val widget = HorizontalLayout(id, nestedModel.widgets)
    copy(
      widgets = widgets :+ widget,
      actions = actions :+ widget.create,
      paths = paths + (id → optionalCurrentWidget) ++ childrenPaths
    )
  }

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

  private def appendWidget(widget: Widget): Model = {
    copy(
      widgets = widgets :+ widget,
      paths = paths + (widget.domId → index(widgets.length)),
      actions = actions :+ widget.create
    )
  }
}
