package com.github.unisay.dancher.widget

import com.github.unisay.dancher.ActionTestHelpers._
import com.github.unisay.dancher.DomArbitraries._
import com.github.unisay.dancher.DomainEvent
import com.github.unisay.dancher.ObservableMatchers._
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.interpreter.JsInterpreter
import com.github.unisay.dancher.widget.Widget._
import monix.execution.schedulers.TestScheduler
import monix.reactive.OverflowStrategy
import monix.reactive.subjects.ConcurrentSubject
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{MustMatchers, PropSpec}
import scala.concurrent.duration._

class TabsSpec extends PropSpec with GeneratorDrivenPropertyChecks with MustMatchers {

  val interpreter = JsInterpreter
  import interpreter._

  val tabs: Widget[DomElemT, TabsModel] = Tabs(const(TabsModel(1)))(
    "Tab 1" -> Label(const("Label")),
    "Tab 2" -> Button(const("Button"), eventTypes = List(Click))
  )

  val model = TabsModel(0)

  property("Tabs widget renders correctly") {
    forAll { (domEvent: DomEvent) =>
      implicit val scheduler = TestScheduler()
      val domEvents = ConcurrentSubject.publish[(String, DomEvent)](OverflowStrategy.Unbounded)

      val renderAction = tabs(model)

      val (element, _, events, script) = renderAction.interpretJs(model, domEvents)

      element mustBe JsInterpreterElement("div0")
      script mustBe
        """
          |var div0 = document.createElement('div');
          |div0.setAttribute("class", "d-vertical");
          |var div1 = document.createElement('div');
          |div1.setAttribute("class", "d-horizontal");
          |var button0 = document.createElement('button');
          |var text0 = document.createTextNode('Tab 1');
          |button0.appendChild(text0);
          |button0.setAttribute("class", "d-button");
          |button0.setAttribute("class", "d-tab");
          |button0.setAttribute("class", "d-tab-active");
          |div1.appendChild(button0);
          |var button1 = document.createElement('button');
          |var text1 = document.createTextNode('Tab 2');
          |button1.appendChild(text1);
          |button1.setAttribute("class", "d-button");
          |button1.setAttribute("class", "d-tab");
          |div1.appendChild(button1);
          |div0.appendChild(div1);
          |var div2 = document.createElement('div');
          |div2.setAttribute("class", "d-vertical");
          |var span0 = document.createElement('span');
          |var text2 = document.createTextNode('Label');
          |span0.appendChild(text2);
          |span0.setAttribute("class", "d-label");
          |div2.appendChild(span0);
          |var button2 = document.createElement('button');
          |var text3 = document.createTextNode('Button');
          |button2.appendChild(text3);
          |button2.setAttribute("class", "d-button");
          |/* HandleEvents(button2) */;
          |button2.getAttribute("class");
          |button2.setAttribute("class", "d-hidden");
          |div2.appendChild(button2);
          |div0.appendChild(div2);
          |/* HandleEvents(button0) */;
          |/* HandleEvents(button1) */;
        """.stripMargin.trim

      def click(element: String) = element -> domEvent
      scheduler.scheduleOnce(100.millis) { domEvents.onNext(click("button0")); () }
      scheduler.scheduleOnce(200.millis) { domEvents.onNext(click("button1")); () }
      scheduler.scheduleOnce(300.millis) { domEvents.onComplete() }

      events.toList() must contain theSameElementsInOrderAs List[(TabsModel, DomainEvent)](
        (TabsModel(0), EffectActionEvent(
          """
            |button0.getAttribute("class");
            |span0.getAttribute("class");
            |span0.setAttribute("class", "d-hidden");
          """.stripMargin.trim)),
        (TabsModel(0), TabActivated(0)),
        (TabsModel(1), EffectActionEvent(
          """
            |button0.getAttribute("class");
            |button1.getAttribute("class");
            |button1.setAttribute("class", "d-tab-active");
            |span0.getAttribute("class");
            |span0.setAttribute("class", "d-hidden");
            |button2.getAttribute("class");
          """.stripMargin.trim)),
      (TabsModel(1), TabActivated(1))
      )
    }
  }

}
