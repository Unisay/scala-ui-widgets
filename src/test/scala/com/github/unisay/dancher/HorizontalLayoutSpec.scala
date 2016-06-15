package com.github.unisay.dancher
/*

import com.github.unisay.dancher.Arbitraries._
import com.github.unisay.dancher.Matchers._
import com.github.unisay.dancher.dom.DomId
import com.github.unisay.dancher.widget.{HorizontalLayout, Label}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class HorizontalLayoutSpec extends Specification with ScalaCheck {

  "HorizontalLayout" should {

    "create" in prop {
      (domId: DomId, label1: Label, label2: Label) ⇒ (label1.domId != label2.domId) ==> {
        HorizontalLayout(domId, Vector(label1, label2)).create must beActionAsScript {
          s"""
             |var div1 = document.createElement('div')
             |div1.setAttribute('id', '${domId.value}')
             |var span2 = document.createElement('span')
             |span2.setAttribute('id', '${label1.domId.value}')
             |span2.setAttribute('class', 'd-label')
             |var text3 = document.createTextNode('${label1.text}')
             |span2.appendChild(text3)
             |var span4 = document.createElement('span')
             |span4.setAttribute('id', '${label2.domId.value}')
             |span4.setAttribute('class', 'd-label')
             |var text5 = document.createTextNode('${label2.text}')
             |span4.appendChild(text5)
             |div1.appendChild(span2)
             |div1.appendChild(span4)
             |div1.setAttribute('class', 'd-horizontal-layout')
           """
        }
      }
    }

    "remove existing child" in prop {
      (domId: DomId, label1: Label, label2: Label) ⇒ (label1.domId != label2.domId) ==> {
        val horizontalLayout = HorizontalLayout(domId, Vector(label1, label2))
        val (updatedLayout, action) = horizontalLayout.removeChild(label2.domId)
        updatedLayout mustEqual HorizontalLayout(domId, Vector(label1))
        action must beActionAsScript {
          s"""
             |var element1 = document.getElementById('${label2.domId.value}')
             |element1.parentNode.removeChild(element1)
          """
        }
      }
    }

    "remove existing child twice" in prop {
      (domId: DomId, label1: Label, label2: Label) ⇒ (label1.domId != label2.domId) ==> {
        val horizontalLayout = HorizontalLayout(domId, Vector(label1, label2))
        val (updatedLayout1, _) = horizontalLayout.removeChild(label2.domId)
        val (updatedLayout2, action2) = updatedLayout1.removeChild(label2.domId)
        updatedLayout1 mustEqual HorizontalLayout(domId, Vector(label1))
        updatedLayout2 mustEqual updatedLayout1
        action2 must beActionAsScript(s"var element1 = document.getElementById('${domId.value}')")
      }
    }

    "remove non-existing child" in prop {
      (domId: DomId, child1: Label, child2: Label) ⇒ (child1.domId != child2.domId) ==> {
        val horizontalLayout = HorizontalLayout(domId, Vector(child1, child2))
        val (updatedLayout, action) = horizontalLayout.removeChild('nonExisting)
        updatedLayout mustEqual horizontalLayout
        action must beActionAsScript(s"var element1 = document.getElementById('${domId.value}')")
      }
    }

  }

}
*/
