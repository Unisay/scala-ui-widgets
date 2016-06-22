package com.github.unisay.dancher

import cats.data.State
import com.github.unisay.dancher.ModelBuilder.{MState, Path}
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.widget._
import monocle.Optional
import monocle.function.Index._

trait AllModelBuilderOps
  extends VerticalLayoutOps
    with HorizontalLayoutOps
    with ButtonOps
    with LabelOps
    with ParagraphOps

object ModelBuilder {
  val instance = ModelBuilder()

  type Path = Optional[WidgetContainer, Widget]
  type MState = State[Model, ActionF[DomBinding]]
}

case class ModelBuilder(state: MState = State((model: Model) ⇒ (model, model.widgetContainer.create))) {

  def map(f: MState ⇒ MState): ModelBuilder = ModelBuilder(f(state))
  def flatMap(f: MState ⇒ ModelBuilder): ModelBuilder = f(state)

  def compose(that: ModelBuilder): ModelBuilder = {
    for {
      thisState ← this
      thatState ← that
      thisAction ← thisState
      thatAction ← thatState
      thisBinding ← thisAction
      thatBinding ← thatAction
      newState ← thisState.transform { case (thisModel, action) ⇒
        thatState.runS()
        val updatedWidgetContainer = thisModel.widgetContainer.appendChild(thatModel.widgetContainer)
        val appendChildAction = thisBinding.element.appendChild(thatBinding.element)
        (thisModel.copy(widgetContainer = updatedWidgetContainer), appendChildAction)
      }
    } yield newState
  }

  def appendWidget(widget: Widget): ModelBuilder =
    appendWidget(widget, widget.create, container ⇒ index[WidgetContainer, Int, Widget](container.children.length))

  def appendWidgetContainer(widgetContainer: WidgetContainer)(nested: ModelBuilder ⇒ ModelBuilder): ModelBuilder = {
    compose(nested(ModelBuilder()))

    /*    val nestedBuilder = f(ModelBuilder())
        val optionalCurrentWidget: Path = index(widgets.length)
        val optionalChildren = optionalCurrentWidget.composeOptional(WidgetContainer._children)
        val childrenPaths = nestedBuilder.paths.mapValues(optionalChildren.composeOptional)
        val widgetContainer = widgetContainerFactory(nestedBuilder.widgets)
        val paths = childrenPaths.updated(widgetContainer.domId, optionalCurrentWidget)
        appendWidget(widgetContainer, widgetContainer.create, paths)*/
  }

  private def appendWidget(widget: Widget, widgetAction: ActionF[DomBinding], f: WidgetContainer ⇒ Path): ModelBuilder = {
    copy(
      state = state.transform { case (model, action) ⇒
        val path = widget.domId → f(model.widgetContainer)
        val modifiedModel = model.copy(
          widgetContainer = model.widgetContainer.appendChild(widget),
          paths = model.paths + path)
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

case class Model(widgetContainer: WidgetContainer, paths: Map[DomId, Path] = Map.empty) {

  def get(id: DomId): Option[Widget] = paths.get(id).flatMap(_.getOption(widgetContainer))

  def modify[W <: Widget](id: DomId)(change: W ⇒ (W, ActionF[DomBinding])): Option[(Model, ActionF[DomBinding])] =
    modifyOpt(id)(change.andThen(Option.apply))

  def modifyOpt[W <: Widget](id: DomId)(change: W ⇒ Option[(W, ActionF[DomBinding])]): Option[(Model, ActionF[DomBinding])] = {
    paths.get(id).flatMap { path ⇒
      path.getOption(widgetContainer).flatMap { widget ⇒
        change(widget.asInstanceOf[W]).map { case (modifiedWidget, modifyAction) ⇒
          (copy(widgetContainer = path.set(modifiedWidget).apply(widgetContainer)), modifyAction)
        }
      }
    }
  }

  def within(id: DomId)(f: Model ⇒ Model): Model = ??? /* TODO implement*/
}
