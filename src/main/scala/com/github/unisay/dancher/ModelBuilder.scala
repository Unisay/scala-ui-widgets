package com.github.unisay.dancher

import cats.data.State
import com.github.unisay.dancher.ModelBuilder.MState
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
  val instance: ModelBuilder = ModelBuilder()

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

        val currentChildPath = _childByIndex(thisModel.widgetContainer.children.length)
        val currentChildContainerPath = currentChildPath ^<-? _containerPrism
        val composedPaths = thisModel.paths ++
          thatModel.paths.mapValues(currentChildContainerPath.composeOptional) +
          thatModel.widgetContainer.domId → currentChildPath

        val composedModel = thisModel.copy(
          widgetContainer = composedWidgetContainer,
          paths = composedPaths
        )

        val composedAction = for {
          thisBinding ← thisAction
          thatBinding ← thatAction
          composedBinding ← thisBinding match { case DomBinding(thisNode, maybeThisEvents) ⇒
            val composedEvents = composeEvents(maybeThisEvents, thatBinding.events)
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
          events = composeEvents(parentBinding.events, widgetBinding.events)
        } yield DomBinding(parentBinding.node, events)
        (modifiedModel, modifiedAction)
      }
    )

  private def composeOptions[T](oa: Option[T], ob: Option[T])(f: (T, T) ⇒ T): Option[T] =
    oa.flatMap(a ⇒ ob.map(b ⇒ f(a, b)).orElse(oa)).orElse(ob)

  private def composeEvents(oa: Option[Observable[ModelEvent]],
                            ob: Option[Observable[ModelEvent]]): Option[Observable[ModelEvent]] =
    composeOptions(oa, ob)((a, b) ⇒ Observable.merge(a, b))


  def build(widgetContainer: WidgetContainer): Frame =
    state.run(Model(widgetContainer)).map { case (model, action) ⇒ Frame(model, action) }.value
}