package com.github.unisay.dancher

import com.github.unisay.dancher.ModelBuilder.Path
import com.github.unisay.dancher.dom.{ActionF, DomId}
import com.github.unisay.dancher.widget.{Widget, WidgetContainer}
import com.github.unisay.dancher.widget.WidgetContainerLenses._

case class Model(widgetContainer: WidgetContainer, paths: Map[DomId, Path] = Map.empty) {

  def get(id: DomId): Option[Widget] = paths.get(id).flatMap(_.getOption(widgetContainer))

  def modify[W <: Widget](id: DomId)(change: W ⇒ (W, ActionF[DomBinding])): Option[Frame] =
    modifyOpt(id)(change.andThen(Option.apply))

  def modifyOpt[W <: Widget](id: DomId)(f: W ⇒ Option[(W, ActionF[DomBinding])]): Option[Frame] = {
    paths.get(id).flatMap { path ⇒
      path.getOption(widgetContainer).flatMap { widget ⇒
        f(widget.asInstanceOf[W]).map { case (modifiedWidget, modifyAction) ⇒
          Frame(
            model = copy(widgetContainer = path.set(modifiedWidget).apply(widgetContainer)),
            action = modifyAction
          )
        }
      }
    }
  }

  def within(id: DomId)(f: ModelBuilder ⇒ ModelBuilder): Option[Frame] = {
    paths.get(id).flatMap { path ⇒
      (path ^<-? _containerPrism).getOption(widgetContainer).map { container: WidgetContainer ⇒
        f(ModelBuilder()).build(container)
      }
    }
  }
}
