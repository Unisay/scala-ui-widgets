package com.github.unisay.dancher

import com.github.unisay.dancher.Model.Path
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.widget._
import monocle.Optional
import monocle.function.Index._
import monocle.std.vector._

trait AllModelOps
  extends VerticalLayoutOps
    with HorizontalLayoutOps
    with ButtonOps
    with LabelOps
    with ParagraphOps

object Model extends AllModelOps {
  type Path = Optional[Vector[Widget], Widget]
  def body = Model(widgets = Vector(Body(Generator[DomId].generate)))
}

case class Model(widgets: Vector[Widget] = Vector.empty,
                 actions: Vector[ActionF[_]] = Vector.empty,
                 paths: Map[DomId, Path] = Map.empty) {

  def get(id: DomId): Option[Widget] = paths.get(id).flatMap(_.getOption(widgets))
  def getAs[W <: Widget](id: DomId): Option[W] = get(id).map(_.asInstanceOf[W])

  def modify[W <: Widget](id: DomId)(change: W ⇒ (W, ActionF[_])): Model = {
    paths.get(id).flatMap { path ⇒
      path.getOption(widgets).map { widget ⇒
        val (modifiedWidget, action) = change(widget.asInstanceOf[W])
        copy(widgets = path.set(modifiedWidget).apply(widgets), actions = actions :+ action)
      }
    }.getOrElse(this)
  }

  def within(id: DomId)(f: Model ⇒ Model): Model = ??? // TODO implement

  def appendWidget(widget: Widget): Model =
    appendWidget(widget, Map(widget.domId → index(widgets.length)))

  def appendWidget(widget: Widget, pathEntries: Map[DomId, Path]): Model =
    copy(widgets = widgets :+ widget, actions = actions :+ widget.create, paths = paths ++ pathEntries)

  def appendWidgetContainer(widgetContainerFactory: Vector[Widget] ⇒ WidgetContainer)(f: Model ⇒ Model): Model = {
    val nestedModel = f(Model())
    val optionalCurrentWidget: Path = index(widgets.length)
    val optionalChildren = optionalCurrentWidget.composeOptional(WidgetContainer._children)
    val childrenPaths = nestedModel.paths.mapValues(optionalChildren.composeOptional)
    val widgetContainer = widgetContainerFactory(nestedModel.widgets)
    val paths = childrenPaths.updated(widgetContainer.domId, optionalCurrentWidget)
    appendWidget(widgetContainer, paths)
  }

}
