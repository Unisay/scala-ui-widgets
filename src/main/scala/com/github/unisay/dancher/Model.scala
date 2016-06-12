package com.github.unisay.dancher

import com.github.unisay.dancher.Model.Path
import com.github.unisay.dancher.dom._
import monocle.Optional
import monocle.function.Index._
import monocle.std.vector._

object Model {
  type Path = Optional[Vector[Widget], Widget]
  def body = Model(widgets = Vector(Body(Generator[DomId].generate)))
}

case class Model(widgets: Vector[Widget] = Vector.empty,
                 actions: Vector[WidgetAction] = Vector.empty,
                 paths: Map[DomId, Path] = Map.empty) {
  import Model._

  def get(id: DomId): Option[Widget] = paths.get(id).flatMap(_.getOption(widgets))
  def getAs[W <: Widget](id: DomId): Option[W] = get(id).map(_.asInstanceOf[W])

  def modify[W <: Widget](id: DomId)(change: W ⇒ (W, WidgetAction)): Model = {
    paths.get(id).flatMap { path ⇒
      path.getOption(widgets).map { widget ⇒
        val (modifiedWidget, action) = change(widget.asInstanceOf[W])
        copy(widgets = path.set(modifiedWidget).apply(widgets), actions = actions :+ action)
      }
    }.getOrElse(this)
  }

  def within(id: DomId)(f: Model ⇒ Model): Model = ???
  def remove(id: DomId): Model = ???

  def vertical(f: Model ⇒ Model)(implicit idGen: Generator[DomId]): Model =
    vertical(idGen.generate)(f)
  def vertical(id: DomId)(f: Model ⇒ Model): Model = {
    val nestedModel = f(Model())
    val optionalCurrentWidget: Path = index(widgets.length)
    val optionalChildren = optionalCurrentWidget.composeOptional(VerticalLayout._children)
    val childrenPaths = nestedModel.paths.mapValues(optionalChildren.composeOptional)
    val widget = VerticalLayout(id, nestedModel.widgets)
    copy(
      widgets = widgets :+ widget,
      actions = actions :+ widget.create,
      paths = paths + (id → optionalCurrentWidget) ++ childrenPaths
    )
  }

  def horizontal(f: Model ⇒ Model)(implicit idGen: Generator[DomId]): Model =
    horizontal(idGen.generate)(f)
  def horizontal(id: DomId)(f: Model ⇒ Model): Model = {
    val nestedModel = f(Model())
    val optionalCurrentWidget: Path = index(widgets.length)
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
