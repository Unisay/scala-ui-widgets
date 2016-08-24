package com.github.unisay.dancher.widget

import com.github.unisay.dancher.ActionTestHelpers._
import com.github.unisay.dancher.ObservableMatchers._
import com.github.unisay.dancher.ClickEvent
import com.github.unisay.dancher.DomArbitraries._
import com.github.unisay.dancher.dom.DomEvent
import com.github.unisay.dancher.interpreter.JsInterpreter.RawElement
import com.github.unisay.dancher.widget.TabsWidget._
import monix.execution.schedulers.TestScheduler
import monix.reactive.subjects.ConcurrentSubject
import monix.reactive.{Observable, OverflowStrategy}
import monocle.macros.Lenses
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{MustMatchers, PropSpec}
import scala.concurrent.duration._

class TabsWidgetSpec extends PropSpec with GeneratorDrivenPropertyChecks with MustMatchers {

  @Lenses case class TabsModel(activeTab: Int)

  val tabs: Widget[TabsModel] = Tabs(TabsModel.activeTab)(
  "Tab 1" -> Label(const("Label")),
  "Tab 2" -> Button(const("Button"), clickHandler = Some(de => Observable(ModelEvent(ClickEvent(de)))))
  )

  val model = TabsModel(0)

  property("Tabs widget renders correctly") {
    forAll { (domEvent: DomEvent) ⇒
      implicit val scheduler = TestScheduler()
      val domEvents = ConcurrentSubject.publish[(String, DomEvent)](OverflowStrategy.Unbounded)

      val (element, modelEvents, script) = tabs.render(model).interpretJs(model, domEvents)

      element mustBe RawElement("div0")
      script mustBe
        """
          |var div0 = document.createElement('div');
          |div0.setAttribute('class', 'd-vertical');
          |var div1 = document.createElement('div');
          |div1.setAttribute('class', 'd-horizontal');
          |var button0 = document.createElement('button');
          |/* SetOnClick(button0) */;
          |button0.setAttribute('class', 'd-button d-tab d-tab-active');
          |var text0 = document.createTextNode('Tab 1');
          |button0.appendChild(text0);
          |div1.appendChild(button0);
          |var button1 = document.createElement('button');
          |/* SetOnClick(button1) */;
          |button1.setAttribute('class', 'd-button d-tab');
          |var text1 = document.createTextNode('Tab 2');
          |button1.appendChild(text1);
          |div1.appendChild(button1);
          |div0.appendChild(div1);
          |var div2 = document.createElement('div');
          |div2.setAttribute('class', 'd-vertical');
          |var span0 = document.createElement('span');
          |span0.setAttribute('class', 'd-label');
          |var text2 = document.createTextNode('Label');
          |span0.appendChild(text2);
          |div2.appendChild(span0);
          |var button2 = document.createElement('button');
          |/* SetOnClick(button2) */;
          |button2.setAttribute('class', 'd-button');
          |var text3 = document.createTextNode('Button');
          |button2.appendChild(text3);
          |button2.setAttribute('class', 'd-hidden');
          |div2.appendChild(button2);
          |div0.appendChild(div2);
        """.stripMargin.trim

      scheduler.scheduleOnce(1.second) { domEvents.onNext("button0" -> domEvent); () }
      scheduler.scheduleOnce(1.second) { domEvents.onNext("button1" -> domEvent); () }

      val future = modelEvents.toListL.runAsync
      scheduler.tickOne()
      scheduler.tickOne()
      scheduler.tickOne()
      scheduler.tickOne()
      scheduler.tickOne()
      scheduler.tickOne()
      scheduler.tickOne()

      modelEvents.toList() must contain theSameElementsInOrderAs List(
        ModelEvent(TabsModel(0), TabActivated(0)),
        ModelEvent(TabsModel(1), TabActivated(1))
      )
    }
  }

}
