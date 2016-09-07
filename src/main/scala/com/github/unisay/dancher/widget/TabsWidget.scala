package com.github.unisay.dancher.widget

import java.lang.Math.{max, min}

import cats.data.Ior
import cats.implicits._
import com.github.unisay.dancher._
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.widget.Widget._
import monix.reactive.Observable
import monocle._
import monocle.macros.Lenses
import org.scalajs.dom.Element

trait TabsWidget extends BasicWidgets with LayoutWidgets {

  @Lenses case class TabsModel(activeTab: Int)

  case class TabActivated[M](index: Int) extends DomainEvent

  def Tabs[M](tabsLens: Lens[M, TabsModel], switchOn: DomEventType = Click)
             (children: (String, Widget[M])*): Widget[M] = {

    def update(oldIndex: Int, newIndex: Int, domElements: Vector[Element],
                 activate: Element => ActionF[_], deactivate: Element => ActionF[_]): EffectAction =
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
          widget.flatMapElement(element => hide(element))
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

      verticalWidget(model).map { verticalBinding ⇒
        verticalBinding.mapDomStream { _ =>
          val buttonBindings = verticalBinding.nested.head.nested
          val buttonElements = buttonBindings.map(_.element)
          val childElements = verticalBinding.nested(1).nested.map(_.element)
          buttonBindings.zipWithIndex.map { case (buttonBinding, index) =>
            buttonBinding.domStream
              .flatMap { case Ior.Left(_) => Observable(index); case _ => Observable.empty }
              .scan(initial = (activeTabIndex, activeTabIndex)) { case ((prev, curr), next) => (curr, next) }
              .map { case (prev, curr) =>
                Ior.Right {
                  update(prev, curr, buttonElements, addClass("d-tab-active"), removeClass("d-tab-active")) followedBy
                    update(prev, curr, childElements, show, hide)
                }
              }
          }.reduce(Observable.merge(_, _))
        }
      }
    }
  }
}

object TabsWidget extends TabsWidget
