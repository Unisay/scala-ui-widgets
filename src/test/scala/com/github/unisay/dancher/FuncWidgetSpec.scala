package com.github.unisay.dancher

import cats.data.Ior
import com.github.unisay.dancher.ActionMatchers._
import com.github.unisay.dancher.ObservableMatchers._
import com.github.unisay.dancher.dom._
import com.github.unisay.dancher.interpreter.JsInterpreter.RawElement
import com.github.unisay.dancher.widget.Widget
import com.github.unisay.dancher.widget.all._
import monix.reactive.Observable
import monocle.Lens
import monocle.macros.Lenses
import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable.Specification

class FuncWidgetSpec(implicit ee: ExecutionEnv) extends Specification {

  sealed trait Gender
  case object Male extends Gender
  case object Female extends Gender

  @Lenses case class User(gender: Gender, name: String, surname: String, address: Address)
  @Lenses case class Address(street: String, building: String)

  val john = User(Male, "John", "Doe", Address("Oak blvd.", "666"))
  val sara = User(Female, "Sara", "Connor", Address("Red Square", "1"))

  "Event propagation" in {
    def domEvent(index: Int): DomEvent = new DomEvent(){ override def toString = s"domEvent$index" }

    val domEvent1 = domEvent(1)
    val domEvent2 = domEvent(2)
    val domEvent3 = domEvent(3)

    val widget: Widget[Unit] = Body +> Tabs(const(1))(
      "Tab 1" -> Header("Tab 1", size = 2),
      "Tab 2" -> Header("Tab 2", size = 2),
      "Tab 3" -> Header("Tab 3", size = 2)
    )
    val renderAction = widget.render(())

    renderAction.interpretJsString(()) must_==
      """
        |var b = document.body;
        |var div0 = document.createElement('div');
        |div0.setAttribute('class', 'd-vertical');
        |var div1 = document.createElement('div');
        |div1.setAttribute('class', 'd-horizontal');
        |var button0 = document.createElement('button');
        |/* SetOnClick(button0) */;
        |button0.setAttribute('class', 'd-button d-tab');
        |var text0 = document.createTextNode('Tab 1');
        |button0.appendChild(text0);
        |div1.appendChild(button0);
        |var button1 = document.createElement('button');
        |/* SetOnClick(button1) */;
        |button1.setAttribute('class', 'd-button d-tab d-tab-active');
        |var text1 = document.createTextNode('Tab 2');
        |button1.appendChild(text1);
        |div1.appendChild(button1);
        |var button2 = document.createElement('button');
        |/* SetOnClick(button2) */;
        |button2.setAttribute('class', 'd-button d-tab');
        |var text2 = document.createTextNode('Tab 3');
        |button2.appendChild(text2);
        |div1.appendChild(button2);
        |div0.appendChild(div1);
        |var div2 = document.createElement('div');
        |div2.setAttribute('class', 'd-vertical');
        |var h20 = document.createElement('h2');
        |var text3 = document.createTextNode('Tab 1');
        |h20.appendChild(text3);
        |h20.setAttribute('class', 'd-hidden');
        |div2.appendChild(h20);
        |var h21 = document.createElement('h2');
        |var text4 = document.createTextNode('Tab 2');
        |h21.appendChild(text4);
        |div2.appendChild(h21);
        |var h22 = document.createElement('h2');
        |var text5 = document.createTextNode('Tab 3');
        |h22.appendChild(text5);
        |h22.setAttribute('class', 'd-hidden');
        |div2.appendChild(h22);
        |div0.appendChild(div2);
        |b.appendChild(div0);
      """.stripMargin.trim

    val (element, modelEvents, _) = renderAction.interpretJs(model = (), domEvents = Observable(
      "button0" -> domEvent1,
      "button1" -> domEvent2,
      "button2" -> domEvent3
    ))

    element must beEqualTo(RawElement("b"))
    modelEvents.toList must contain(exactly(
      Ior.both[Unit, DomainEvent]((), TabActivated[Unit](0)),
      Ior.both[Unit, DomainEvent]((), TabActivated[Unit](1)),
      Ior.both[Unit, DomainEvent]((), TabActivated[Unit](2))
    ))
  }

  "Library widgets" in {

    def Header(): Widget[User] = {
      Vertical(
        Horizontal(
          Label(User.name) > Label(User.surname)
        ) >
        Horizontal(
          Label(User.address ^|-> Address.street) > Label(User.address ^|-> Address.building)
        )
      )
    }

    val action = Header().render(john)

    action.interpretJsString mustEqual
      """
        |var div0 = document.createElement('div');
        |div0.setAttribute('class', 'd-vertical');
        |var div1 = document.createElement('div');
        |div1.setAttribute('class', 'd-horizontal');
        |var span0 = document.createElement('span');
        |span0.setAttribute('class', 'd-label');
        |var text0 = document.createTextNode('John');
        |span0.appendChild(text0);
        |div1.appendChild(span0);
        |var span1 = document.createElement('span');
        |span1.setAttribute('class', 'd-label');
        |var text1 = document.createTextNode('Doe');
        |span1.appendChild(text1);
        |div1.appendChild(span1);
        |div0.appendChild(div1);
        |var div2 = document.createElement('div');
        |div2.setAttribute('class', 'd-horizontal');
        |var span2 = document.createElement('span');
        |span2.setAttribute('class', 'd-label');
        |var text2 = document.createTextNode('Oak blvd.');
        |span2.appendChild(text2);
        |div2.appendChild(span2);
        |var span3 = document.createElement('span');
        |span3.setAttribute('class', 'd-label');
        |var text3 = document.createTextNode('666');
        |span3.appendChild(text3);
        |div2.appendChild(span3);
        |div0.appendChild(div2);
      """.stripMargin.trim
  }

  "Domain specific widgets" in {

    object AddressBar {
      def apply(): Widget[Address] =
        Horizontal(Label(Address.street) > Label(Address.building))

      def apply[U](l: Lens[U, Address]): Widget[U] =
        Horizontal(Label(l ^|-> Address.street) > Label(l ^|-> Address.building))
    }

    object Badge {
      /* Example of conditional logic */
      def apply(): Widget[User] = new Widget[User] {
        def render(user: User) = {
          val prefix = user.gender match {
            case Male => "Sir"
            case Female => "Lady"
          }
          Horizontal (
            Label(const(prefix)) > Label(User.name) > Label(User.surname), cssClasses = List("badge")
          ).render(user)
        }
      }
    }

    object Header {
      def apply(): Widget[User] =
        Vertical (
          Badge() >
          AddressBar(User.address)
        )
    }

    val action = Header().render(john)

    action.interpretJsString mustEqual
      """
        |var div0 = document.createElement('div');
        |div0.setAttribute('class', 'd-vertical');
        |var div1 = document.createElement('div');
        |div1.setAttribute('class', 'd-horizontal badge');
        |var span0 = document.createElement('span');
        |span0.setAttribute('class', 'd-label');
        |var text0 = document.createTextNode('Sir');
        |span0.appendChild(text0);
        |div1.appendChild(span0);
        |var span1 = document.createElement('span');
        |span1.setAttribute('class', 'd-label');
        |var text1 = document.createTextNode('John');
        |span1.appendChild(text1);
        |div1.appendChild(span1);
        |var span2 = document.createElement('span');
        |span2.setAttribute('class', 'd-label');
        |var text2 = document.createTextNode('Doe');
        |span2.appendChild(text2);
        |div1.appendChild(span2);
        |div0.appendChild(div1);
        |var div2 = document.createElement('div');
        |div2.setAttribute('class', 'd-horizontal');
        |var span3 = document.createElement('span');
        |span3.setAttribute('class', 'd-label');
        |var text3 = document.createTextNode('Oak blvd.');
        |span3.appendChild(text3);
        |div2.appendChild(span3);
        |var span4 = document.createElement('span');
        |span4.setAttribute('class', 'd-label');
        |var text4 = document.createTextNode('666');
        |span4.appendChild(text4);
        |div2.appendChild(span4);
        |div0.appendChild(div2);
      """.stripMargin.trim

    val actionSara = Header().render(sara)

    actionSara.interpretJsString mustEqual
      """
        |var div0 = document.createElement('div');
        |div0.setAttribute('class', 'd-vertical');
        |var div1 = document.createElement('div');
        |div1.setAttribute('class', 'd-horizontal badge');
        |var span0 = document.createElement('span');
        |span0.setAttribute('class', 'd-label');
        |var text0 = document.createTextNode('Lady');
        |span0.appendChild(text0);
        |div1.appendChild(span0);
        |var span1 = document.createElement('span');
        |span1.setAttribute('class', 'd-label');
        |var text1 = document.createTextNode('Sara');
        |span1.appendChild(text1);
        |div1.appendChild(span1);
        |var span2 = document.createElement('span');
        |span2.setAttribute('class', 'd-label');
        |var text2 = document.createTextNode('Connor');
        |span2.appendChild(text2);
        |div1.appendChild(span2);
        |div0.appendChild(div1);
        |var div2 = document.createElement('div');
        |div2.setAttribute('class', 'd-horizontal');
        |var span3 = document.createElement('span');
        |span3.setAttribute('class', 'd-label');
        |var text3 = document.createTextNode('Red Square');
        |span3.appendChild(text3);
        |div2.appendChild(span3);
        |var span4 = document.createElement('span');
        |span4.setAttribute('class', 'd-label');
        |var text4 = document.createTextNode('1');
        |span4.appendChild(text4);
        |div2.appendChild(span4);
        |div0.appendChild(div2);
      """.stripMargin.trim
  }

}
