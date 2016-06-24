package com.github.unisay.dancher

import cats.data.State
import com.github.unisay.dancher.ModelBuilder.{MState, Path}
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.widget.WidgetContainerLenses._
import com.github.unisay.dancher.widget._
import monix.reactive.Observable
import monocle.Optional

trait AllModelBuilderOps
  extends VerticalLayoutOps
    with HorizontalLayoutOps
    with ButtonOps
    with LabelOps
    with ParagraphOps

object ModelBuilder extends AllModelBuilderOps {
  val instance = ModelBuilder()

  type Path = Optional[WidgetContainer, Widget]
  type MState = State[Model, ActionF[DomBinding]]
}

case class ModelBuilder(state: MState = State((model: Model) ⇒ (model, model.widgetContainer.create)))
  extends AllModelBuilderOps {

  def map(f: MState ⇒ MState): ModelBuilder = ModelBuilder(f(state))
  def flatMap(f: MState ⇒ ModelBuilder): ModelBuilder = f(state)

  def appendWidgetContainer(widgetContainer: WidgetContainer)(nested: ModelBuilder ⇒ ModelBuilder): ModelBuilder = {
    val thatBuilder = nested(ModelBuilder())
    map {
      _.transform { case (thisModel, thisAction) ⇒
        val (thatModel, thatAction) = thatBuilder.state.run(Model(widgetContainer)).value

        val composedWidgetContainer = thisModel.widgetContainer.appendChild(thatModel.widgetContainer)

        val currentChildPath = _childByIndex(thisModel.widgetContainer.children.length) ^<-? _containerPrism
        val composedPaths = thatModel.paths.mapValues(currentChildPath.composeOptional)

        val composedModel = thisModel.copy(widgetContainer = composedWidgetContainer, paths = composedPaths)

        val composedAction = for {
          thisBinding ← thisAction
          thatBinding ← thatAction
          composedBinding ← thisBinding match { case DomBinding(thisNode, maybeThisEvents) ⇒
            val composedEvents = composeOptions(maybeThisEvents, thatBinding.events)((l, r) ⇒ Observable.merge(l, r))
            thisNode.appendChild(thatBinding.node).map(node ⇒ DomBinding(node, composedEvents))
          }
        } yield composedBinding

        (composedModel, composedAction)
      }
    }
  }

  def appendWidget(widget: Widget): ModelBuilder = appendWidget(widget, widget.create)

  def appendWidget(widget: Widget, widgetAction: ActionF[DomBinding]): ModelBuilder =
    copy(
      state = state.transform { case (model, action) ⇒
        val childIdx = model.widgetContainer.children.length
        val path = widget.domId → _childByIndex(childIdx)
        val modifiedWidgetContainer = model.widgetContainer.appendChild(widget)
        val modifiedModel = model.copy(widgetContainer = modifiedWidgetContainer, paths = model.paths + path)
        val modifiedAction = for {
          parentBinding ← action
          widgetBinding ← widgetAction
          _ ← parentBinding.node.appendChild(widgetBinding.node)
        } yield parentBinding
        (modifiedModel, modifiedAction)
      }
    )

  private def composeOptions[T](oa: Option[T], ob: Option[T])(f: (T, T) ⇒ T): Option[T] =
    oa.flatMap(a ⇒ ob.map(b ⇒ f(a, b)).orElse(oa)).orElse(ob)
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
