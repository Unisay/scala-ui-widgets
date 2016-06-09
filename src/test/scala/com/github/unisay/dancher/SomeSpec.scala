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

      val bodyModel = BodyModel()
        .vertical('labels) { _
          .button(label = "Add Item",    onClick = AddItem)
          .button(label = "Remove Item", onClick = RemItem)
          .button(label = "Update Foo",  onClick = UpdateLabel)
        }
        .horizontal { _
          .label('foo, "Foo")
          .vertical('vertical) { _
            .label("Bar")
            .label(id = 'baz, text = "Baz")
          }
        }

      bodyModel.actions.head must beActionAsScript(
        s"""
        |var div1 = document.createElement('div')
        |var span2 = document.createElement('span')
        |span2.setAttribute('id', 'foo')
        |span2.setAttribute('class', 'd-label')
        |var text3 = document.createTextNode('Foo')
        |span2.appendChild(text3)
        |var div4 = document.createElement('div')
        |var span5 = document.createElement('span')
        |span5.setAttribute('id', 'a98103ed-cb25-4499-9ddc-2403af1c42c3')
        |span5.setAttribute('class', 'd-label')
        |var text6 = document.createTextNode('Bar')
        |span5.appendChild(text6)
        |var span7 = document.createElement('span')
        |span7.setAttribute('id', 'baz')
        |span7.setAttribute('class', 'd-label')
        |var text8 = document.createTextNode('Baz')
        |span7.appendChild(text8)
        |div4.appendChild(span5)
        |div4.appendChild(span7)
        |div4.setAttribute('class', 'd-vertical-layout')
        |div1.appendChild(span2)
        |div1.appendChild(div4)
        |div1.setAttribute('class', 'd-horizontal-layout')
        """
      )
    }

  }
}
