package com.github.unisay.dancher

import com.github.unisay.dancher.ActionTestHelpers._
import com.github.unisay.dancher.widget.Widget
import com.github.unisay.dancher.widget.Widget._
import monix.execution.schedulers.TestScheduler
import monocle.Lens
import monocle.macros.Lenses
import org.scalatest.{FlatSpec, MustMatchers}

class FuncWidgetSpec extends FlatSpec with MustMatchers {

  implicit val scheduler = TestScheduler()

  sealed trait Gender
  case object Male extends Gender
  case object Female extends Gender

  @Lenses case class User(gender: Gender, name: String, surname: String, address: Address)
  @Lenses case class Address(street: String, building: String)

  val john = User(Male, "John", "Doe", Address("Oak blvd.", "666"))
  val sara = User(Female, "Sara", "Connor", Address("Red Square", "1"))

  "Library widgets" must "render correctly" in {

    def Header: Widget[User] = {
      Vertical(
        Horizontal(
          Label(User.name) > Label(User.surname)
        ) >
        Horizontal(
          Label(User.address ^|-> Address.street) > Label(User.address ^|-> Address.building)
        )
      )
    }

    val action = Header(john)

    action.interpretJsString(john) mustEqual
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

  "Domain specific widgets" must "render correctly" in {

    object AddressBar {
      def apply(): Widget[Address] =
        Horizontal(Label(Address.street) > Label(Address.building))

      def apply[U](l: Lens[U, Address]): Widget[U] =
        Horizontal(Label(l ^|-> Address.street) > Label(l ^|-> Address.building))
    }

    object Badge {
      /* Example of conditional logic */
      def apply(): Widget[User] = Widget { (user: User) =>
        val prefix = user.gender match {
          case Male => "Sir"
          case Female => "Lady"
        }
        Horizontal (
          Label(const(prefix)) > Label(User.name) > Label(User.surname), cssClasses = List("badge")
        ).apply(user)
      }
    }

    def Header: Widget[User] =
      Vertical (
        Badge() >
        AddressBar(User.address)
      )

    val action = Header(john)

    action.interpretJsString(john) mustEqual
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

    val actionSara = Header(sara)

    actionSara.interpretJsString(sara) mustEqual
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
