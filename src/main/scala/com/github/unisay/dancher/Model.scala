package com.github.unisay.dancher

import com.github.unisay.dancher.Model.Path
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.widget._
import monocle._
import monocle.function.Index._
import monocle.std.vector._
import Label._
import Button._
import HorizontalLayout._
import VerticalLayout._

object Model {
  type Path = Vector[SomeWidget] Optional SomeWidget
  def body = Model(widgets = Vector(Body(Generator[DomId].generate)))
  def _children: SomeWidget Optional Vector[SomeWidget] = { 
    val _getOption: SomeWidget ⇒ Option[Vector[SomeWidget]] = someWidget ⇒
      someWidget.container.map { container ⇒
        container.children(someWidget.instance)
      }
    val _set: Vector[SomeWidget] ⇒ SomeWidget ⇒ SomeWidget =
      sww ⇒ sw ⇒ sw.container.fold(sw)(c ⇒ SomeWidget(c.setChildren(sw.instance)(sww))(sw.widget, c)) 
    Optional(_getOption)(_set)
  }
}

case class Model(widgets: Vector[SomeWidget] = Vector.empty,
                 actions: Vector[ActionF[_]] = Vector.empty,
                 paths: Map[DomId, Path] = Map.empty) {
  import Model._

  def get(id: DomId): Option[SomeWidget] = paths.get(id).flatMap(_.getOption(widgets))
  def getAs(id: DomId): Option[SomeWidget] = get(id)

  def modify(id: DomId)(f: SomeWidget ⇒ (SomeWidget, ActionF[_])): Model = {
    paths.get(id).flatMap { path ⇒
      path.getOption(widgets).map { isWidget ⇒
        val (modifiedWidget, action) = f(isWidget)
        val someWidgets = path.set(modifiedWidget).apply(widgets)
        copy(widgets = someWidgets, actions = actions :+ action)
      }
    }.getOrElse(this)
  }

  def within(id: DomId)(f: Model ⇒ Model): Model = ???

  def vertical(f: Model ⇒ Model)(implicit idGen: Generator[DomId]): Model =
    vertical(idGen.generate)(f)

  def vertical(id: DomId)(f: Model ⇒ Model): Model = {
    val nestedModel = f(Model())
    val optionalCurrentWidget: Path = index(widgets.length)
    val optionalChildren = optionalCurrentWidget.composeOptional(_children)
    val childrenPaths: Map[DomId, Path] = nestedModel.paths.mapValues(optionalChildren.composeOptional)
    val widget = SomeWidget(VerticalLayout(id, nestedModel.widgets))
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
    val optionalChildren = optionalCurrentWidget.composeOptional(_children)
    val childrenPaths: Map[DomId, Path] = nestedModel.paths.mapValues(optionalChildren.composeOptional)
    val widget = SomeWidget(HorizontalLayout(id, nestedModel.widgets))
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

  private def appendWidget[W: Widget](widget: W): Model = {
    val isWidget = SomeWidget(widget)
    copy(
      widgets = widgets :+ isWidget,
      paths = paths + (isWidget.domId → index(widgets.length)),
      actions = actions :+ isWidget.create
    )
  }
}
