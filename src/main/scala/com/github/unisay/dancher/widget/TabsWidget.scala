package com.github.unisay.dancher.widget

import java.lang.Math.{max, min}

import cats.implicits._
import com.github.unisay.dancher._
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.interpreter.ActionInterpreter
import com.github.unisay.dancher.widget.Widget._
import monix.reactive.Observable
import monocle._
import monocle.macros.Lenses

trait TabsWidget extends BasicWidgets with LayoutWidgets {

  @Lenses case class TabsModel(activeTab: Int)

  case class TabActivated[M](index: Int) extends DomainEvent

  def Tabs[M](tabsLens: Lens[M, TabsModel], switchOn: DomEventType = Click)
             (children: (String, Widget[M])*)
             (implicit interpreter: ActionInterpreter): Widget[M] = {

    def activate[E: DomElem](oldIndex: Int, newIndex: Int, domElements: Iterable[E],
                             activate: E => ActionF[_], deactivate: E => ActionF[_]): EffectAction =
      domElements.zipWithIndex.map {
        case (domElement, index) if oldIndex == index => deactivate(domElement).void
        case (domElement, index) if newIndex == index => activate(domElement).void
        case _ => noAction[Unit]
      }.reduce(_ followedBy _)

    val childrenList = children.toList
    // Using childrenList.unzip breaks on JS engine :(
    val labels = childrenList.map(_._1)
    val widgets = childrenList.map(_._2)
    val activeTabIndexLens = tabsLens ^|-> TabsModel.activeTab

    Widget { (model: M) =>
      val activeTabIndex = max(min(activeTabIndexLens.get(model), children.length), 0)

      val childrenWidgets: List[Widget[M]] = widgets.zipWithIndex.map {
        case (widget, index) if index != activeTabIndex =>
          widget.flatMapElement((e, ev) => hide(e)(ev))
        case (widget, _) => widget
      }

      val buttonWidgets: List[Widget[M]] = labels.zipWithIndex.map { case (label, index) =>
        Button[M](const(label),
          eventTypes = List(Click),
          cssClasses = "d-tab" :: (if (index == activeTabIndex) List("d-tab-active") else Nil))
      }

      val verticalWidget = Vertical(
        Horizontal(buttonWidgets) >
        Vertical(childrenWidgets)
      )

      def buttonEventsHandler[E: DomElem](index: Int, buttonElements: Iterable[E], childElements: Iterable[E]) =
        DomEventHandlers.On[M](switchOn) { (eventModel: M, event: DomEvent) =>
          val oldIndex = activeTabIndexLens.get(eventModel)
          if (index =!= oldIndex) {
            val updatedModel = activeTabIndexLens.set(index)(eventModel)
            val updateButtons = activate[E](oldIndex, index, buttonElements,
              addClass("d-tab-active"), removeClass("d-tab-active"))
            val updateChildren = activate[E](oldIndex, index, childElements, show, hide)
            val clickAction = updateButtons followedBy updateChildren
            val result = HandlerResult(updatedModel, TabActivated(index), clickAction)
            Observable(result)
          } else Observable.empty
        }

      for {
        verticalBinding <- verticalWidget(model)
        buttonBindings = verticalBinding.nested.head.nested
        childElements = verticalBinding.nested(1).nested.map(_.element)
        buttonsEvents <- buttonBindings.zipWithIndex.map { case (binding, index) =>
          import binding.elementEvidence
          handleEvents(binding.element, buttonEventsHandler(index, buttonBindings.map(_.element), childElements))
        }.sequence
        mergedEvents = buttonsEvents.reduce(Observable.merge(_, _))
      } yield DomBinding(verticalBinding.element, Vector.empty, mergedEvents)(verticalBinding.elementEvidence)
    }
  }
}

object TabsWidget extends TabsWidget
