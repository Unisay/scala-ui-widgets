package com.github.unisay.dancher

import com.github.unisay.dancher.Matchers._
import com.github.unisay.dancher.dom.DomEvent
import org.specs2.mutable.Specification

class SomeSpec extends Specification {

  case class AddItem(event: DomEvent) extends DomainEvent
  case class RemItem(event: DomEvent) extends DomainEvent
  case class UpdateLabel(event: DomEvent) extends DomainEvent

  "BodyModel" should {

    "create" in {

      val bodyModel = ModelBuilder.body
        .vertical('labels) { _
          .button(label = "Add Item")
          .button(label = "Remove Item")
          .button(label = "Update Foo")
        }
        .horizontal { _
          .label('foo, "Foo")
          .vertical('vertical) { _
            .label("Bar")
            .label(id = 'baz, text = "Baz")
          }
        }

      bodyModel.actions.head must beActionAsScript(
        """
        |var div1 = document.createElement('div')
        |var button2 = document.createElement('button')
        |button2.setAttribute('id', '7acd2fd1-ced2-40c5-92c6-dbad0c508b84')
        |button2.setAttribute('class', 'd-button')
        |var text3 = document.createTextNode('Add Item')
        |button2.appendChild(text3)
        |/* SetOnClick(button2) */
        |var button4 = document.createElement('button')
        |button4.setAttribute('id', 'b6e37903-5e3e-48aa-bf14-9bccbdf11cc7')
        |button4.setAttribute('class', 'd-button')
        |var text5 = document.createTextNode('Remove Item')
        |button4.appendChild(text5)
        |/* SetOnClick(button4) */
        |var button6 = document.createElement('button')
        |button6.setAttribute('id', '24990dd7-a7d9-4c42-8e5d-8647457ddf43')
        |button6.setAttribute('class', 'd-button')
        |var text7 = document.createTextNode('Update Foo')
        |button6.appendChild(text7)
        |/* SetOnClick(button6) */
        |div1.appendChild(button2)
        |div1.appendChild(button4)
        |div1.appendChild(button6)
        |div1.setAttribute('class', 'd-vertical-layout')
        """
      )
    }

  }
}
