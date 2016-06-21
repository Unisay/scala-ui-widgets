package com.github.unisay.dancher

import cats.data.State
import com.github.unisay.dancher.ModelBuilder.Path
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

object ModelBuilder extends AllModelOps {
  type Path = Optional[Vector[Widget], Widget]
  def body = {
    val widget = Body(Generator[DomId].generate)
    val model = Model(widgets = Vector(widget))
    ModelBuilder(State.set(model).map(_ ⇒ widget.create))
  }
}

case class ModelBuilder(state: State[Model, ActionF[DomBinding]] = State.pure(noAction)) {

  def appendWidget(widget: Widget): ModelBuilder =
    appendWidget(widget, widget.create, widgets ⇒ index[Vector[Widget], Int, Widget](widgets.length))

  def appendWidgetContainer(widgetContainerFactory: Vector[Widget] ⇒ WidgetContainer)
                           (f: ModelBuilder ⇒ ModelBuilder): ModelBuilder = {
    val nestedBuilder = f(ModelBuilder())
    val optionalCurrentWidget: Path = index(widgets.length)
    val optionalChildren = optionalCurrentWidget.composeOptional(WidgetContainer._children)
    val childrenPaths = nestedBuilder.paths.mapValues(optionalChildren.composeOptional)
    val widgetContainer = widgetContainerFactory(nestedBuilder.widgets)
    val paths = childrenPaths.updated(widgetContainer.domId, optionalCurrentWidget)
    appendWidget(widgetContainer, widgetContainer.create, paths)
  }

  private def appendWidget(widget: Widget, widgetAction: ActionF[DomBinding], f: Vector[Widget] ⇒ Path): ModelBuilder = {
    copy(
      state = state.transform { case (model, action) ⇒
        val path = widget.domId → f(model.widgets)
        val modifiedModel = model.copy(widgets = model.widgets :+ widget, paths = model.paths + path)
        val modifiedAction = for {
          parentBinding ← action
          widgetBinding ← widgetAction
          _ ← parentBinding.element.appendChild(widgetBinding.element)
        } yield parentBinding
        (modifiedModel, modifiedAction)
      }
    )
  }

}

case class Model(widgets: Vector[Widget] = Vector.empty, paths: Map[DomId, Path] = Map.empty) {

  def get(id: DomId): Option[Widget] = paths.get(id).flatMap(_.getOption(widgets))

  def modify[W <: Widget](id: DomId)(change: W ⇒ (W, ActionF[DomBinding])): Option[(Model, ActionF[DomBinding])] =
    modifyOpt(id)(change.andThen(Option.apply))

  def modifyOpt[W <: Widget](id: DomId)(change: W ⇒ Option[(W, ActionF[DomBinding])]): Option[(Model, ActionF[DomBinding])] = {
    paths.get(id).flatMap { path ⇒
      path.getOption(widgets).flatMap { widget ⇒
        change(widget.asInstanceOf[W]).map { case (modifiedWidget, modifyAction) ⇒
          (copy(widgets = path.set(modifiedWidget).apply(widgets)), modifyAction)
        }
      }
    }
  }

  def within(id: DomId)(f: Model ⇒ Model): Model = ??? /* TODO implement*/
}
